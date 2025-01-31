(* https://protohackers.com *)

let ( let* ) = Lwt.bind

module type Lwt_socket_in = sig
  val buff_len : int
  val socket : Lwt_unix.file_descr
end

module Lwt_socket (In : Lwt_socket_in) = struct
  let read ?stop_cond () =
    let stop_cond =
      match stop_cond with Some x -> x | None -> fun _ -> false
    in
    let buff = Bytes.create In.buff_len in
    let rec go acc =
      let* n = Lwt_unix.read In.socket buff 0 In.buff_len in
      let chunk = Bytes.sub buff 0 n in
      let stop = stop_cond @@ String.of_bytes chunk in
      let acc = chunk :: acc in
      if n = 0 || stop then
        acc |> List.rev |> Bytes.concat Bytes.empty |> Lwt.return
      else go acc
    in
    go []

  let write buff =
    let total = Bytes.length buff in
    let rec go left =
      if left = 0 then Lwt.return_unit
      else
        let pos = total - left in
        let* n = Lwt_unix.write In.socket buff pos left in
        go (left - n)
    in
    go total
end

type json =
  | JNull
  | JInt of int
  | JFloat of float
  | JBool of bool
  | JString of string
  | JArray of json list
  | JObject of (string * json) list

let json_of_string str =
  let char_at i = try Some (String.get str i) with _ -> None in
  let char_at_eq i cmp =
    match char_at i with Some x -> x = cmp | _ -> false
  in
  let rec skip i =
    match char_at i with
    | Some ' ' | Some '\n' | Some '\r' | Some '\t' -> skip (i + 1)
    | _ -> i
  in
  let rec parse i =
    let rec parse' parsers i acc =
      match parsers with
      | [] -> (i, acc)
      | p :: ps -> (
          match acc with
          | Some _ -> (i, acc)
          | None ->
              let i = skip i in
              let j, json = p i in
              parse' ps j @@ if i = j then None else Some json)
    in
    let parsers =
      [
        parse_object;
        parse_array;
        parse_true;
        parse_false;
        parse_null;
        parse_string;
        parse_float;
        parse_int;
      ]
    in
    parse' parsers i None
  and parse_int i =
    let ptr = i in
    let rec parse_int' i acc state =
      let slurp c = (acc * 10) + int_of_char c - 48 in
      match char_at i with
      | Some '-' when state = 0 -> parse_int' (i + 1) acc 1
      | Some c when c >= '0' && c <= '9' && (state = 0 || state = 2) ->
          parse_int' (i + 1) (slurp c) 2
      | Some c when c >= '0' && c <= '9' && state = 1 ->
          parse_int' (i + 1) (slurp c) 1
      | _ when state = 1 -> (i, JInt (acc * -1))
      | _ when state = 2 -> (i, JInt acc)
      | _ -> (ptr, JInt acc)
    in
    parse_int' i 0 0
  and parse_float i =
    let float_of x y = float_of_string @@ Printf.sprintf "%d.%d" x y in
    let ptr = i in
    let rec parse_float' i acc state =
      let j, res = parse_int i in
      match res with
      | JInt first when j <> i && state = 0 -> parse_float' j first 1
      | _ when char_at_eq i '.' && state = 1 -> parse_float' (i + 1) acc 2
      | JInt second when j <> i && state = 2 -> (j, JFloat (float_of acc second))
      | _ -> (ptr, JFloat 0.)
    in
    parse_float' i 0 0
  and parse_string i =
    let ptr = i in
    let rec parse_string' i acc state =
      let slurp c = Printf.sprintf "%s%c" acc c in
      match char_at i with
      | Some '"' when state = 0 -> parse_string' (i + 1) acc 1
      | Some '"' -> (i + 1, JString acc)
      | Some c when state = 1 -> parse_string' (i + 1) (slurp c) state
      | _ -> (ptr, JString acc)
    in
    parse_string' i "" 0
  and parse_keyword i kw =
    let ptr = i in
    let rec parse_keyword' i acc curr_len len =
      let slurp c = Printf.sprintf "%s%c" acc c in
      match char_at i with
      | Some c when curr_len < len ->
          parse_keyword' (i + 1) (slurp c) (curr_len + 1) len
      | _ when curr_len = len -> (i, acc)
      | _ -> (ptr, acc)
    in
    let j, str = parse_keyword' i "" 0 (String.length kw) in
    if j <> i && str = kw then Some (j, kw) else None
  and parse_null i =
    match parse_keyword i "null" with
    | Some (j, _) -> (j, JNull)
    | None -> (i, JNull)
  and parse_true i =
    match parse_keyword i "true" with
    | Some (j, _) -> (j, JBool true)
    | None -> (i, JBool true)
  and parse_false i =
    match parse_keyword i "false" with
    | Some (j, _) -> (j, JBool false)
    | None -> (i, JBool false)
  and parse_array i =
    let ptr = i in
    let rec parse_array' i acc state =
      let i = skip i in
      match char_at i with
      | (Some '[' | Some ',') when state = 0 -> parse_array' (i + 1) acc 1
      | Some ']' -> (i + 1, JArray (List.rev acc))
      | Some _ when state = 1 -> (
          let j, res = parse i in
          match res with
          | Some json -> parse_array' j (json :: acc) 0
          | None -> (ptr, JArray acc))
      | _ -> (ptr, JArray acc)
    in
    parse_array' i [] 0
  and parse_object i =
    let ptr = i in
    let rec parse_object' i acc state key =
      let i = skip i in
      match char_at i with
      | (Some '{' | Some ',') when state = 0 -> parse_object' (i + 1) acc 1 key
      | Some '}' -> (i + 1, JObject (List.rev acc))
      | Some '"' when state = 1 -> (
          let j, json = parse_string i in
          match json with
          | JString key when i <> j -> parse_object' j acc 2 key
          | _ -> (ptr, JObject acc))
      | Some ':' when state = 2 -> (
          let j, res = parse (i + 1) in
          match res with
          | Some json -> parse_object' j ((key, json) :: acc) 0 ""
          | None -> (ptr, JObject acc))
      | _ -> (ptr, JObject acc)
    in
    parse_object' i [] 0 ""
  in
  let i, res = parse 0 in
  let j = skip i in
  if j = String.length str then res
  else (
    Printf.printf "failed parsing at pos %d char '%c'\n" j (String.get str j);
    None)

(* problem 0 *)
let _echo client =
  let module S = Lwt_socket (struct
    let buff_len = 0x100
    let socket = client
  end) in
  let* msg = S.read () in
  S.write msg

(* problem 1 *)
let json client =
  let module S = Lwt_socket (struct
    let buff_len = 0x100
    let socket = client
  end) in
  let is_prime n =
    try
      let n = int_of_string n in
      let rec go d =
        if d > n / 2 then true else if n mod d = 0 then false else go (d + 1)
      in
      go 2
    with _ -> false
  in
  let is_primef n =
    try
      let n = int_of_float @@ float_of_string n in
      let rec go d =
        if d > n / 2 then true else if n mod d = 0 then false else go (d + 1)
      in
      go 2
    with _ -> false
  in
  let contains s1 s2 =
    try
      let len = String.length s2 in
      for i = 0 to String.length s1 - len do
        if String.sub s1 i len = s2 then raise Exit
      done;
      false
    with Exit -> true
  in
  let get_number_opt (req : json) =
    match req with
    | JObject pairs -> (
        let method_opt = List.find_opt (fun (key, _) -> key = "method") pairs in
        let number_opt = List.find_opt (fun (key, _) -> key = "number") pairs in
        match (method_opt, number_opt) with
        | Some (_, JString m), Some (_, JInt jint) when contains m "isPrime" ->
            Some (string_of_int jint)
        | Some (_, JString m), Some (_, JFloat jfloat) when contains m "isPrime"
          ->
            Some (string_of_float jfloat)
        | _ -> None)
    | _ -> None
  in
  let bytes str = Bytes.of_string str in
  let* msg = S.read ~stop_cond:(fun x -> String.ends_with ~suffix:"\n" x) () in
  let* () = Lwt_io.printl @@ String.of_bytes msg in
  let json_opt = json_of_string @@ String.of_bytes msg in
  match json_opt with
  | None -> S.write msg
  | Some json -> (
      match get_number_opt json with
      | None -> S.write msg
      | Some n ->
          if is_prime n || is_primef n then
            S.write @@ bytes "{\"method\":\"isPrime\",\"prime\":true}\n"
          else S.write @@ bytes "{\"method\":\"isPrime\",\"prime\":false}\n")

let rec accept_loop server_socket =
  let* client, _ = Lwt_unix.accept server_socket in
  Lwt.async (fun () ->
      let* () = json client in
      Lwt_unix.close client);
  accept_loop server_socket

let start_server ~port ~backlog =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket backlog;
  let* () = Lwt_io.printlf "listening on port %d - backlog %d" port backlog in
  accept_loop server_socket

let () = Lwt_main.run (start_server ~port:3000 ~backlog:128)
