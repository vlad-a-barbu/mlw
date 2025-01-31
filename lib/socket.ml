let buff_len = 4096

let read ?stop_cond sock =
  let stop_cond = match stop_cond with Some x -> x | None -> fun _ -> false in
  let buff = Bytes.create buff_len in
  let rec go acc =
    let n = Unix.read sock buff 0 buff_len in
    let chunk = Bytes.sub_string buff 0 n in
    if n = 0 || stop_cond chunk then List.rev (chunk :: acc)
    else go (chunk :: acc)
  in
  String.concat "" @@ go []

let write sock str =
  let buff = Bytes.of_string str in
  let buff_len = Bytes.length buff in
  let rec go left =
    let pos = buff_len - left in
    let n = Unix.write sock buff pos left in
    if n = 0 then () else go (left - n)
  in
  go buff_len
