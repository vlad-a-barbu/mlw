module Socket = Mlw.Socket

let stdin_line () =
  try match In_channel.input_line stdin with Some line -> line | None -> ""
  with _ -> ""

let () =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock @@ ADDR_INET (inet_addr_loopback, 3000);
  let cleanup () = close sock in
  let rec go () =
    let req = stdin_line () in
    if String.length req = 0 then ()
    else (
      Socket.writeline sock req;
      Socket.readline sock |> print_endline;
      go ())
  in
  Fun.protect ~finally:cleanup go
