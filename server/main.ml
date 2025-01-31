module S = Mlw.Scheduler
module Socket = Mlw.Socket

type server_config = {
  port : int;
  backlog : int;
  handler : Unix.file_descr -> unit;
}

let server cfg =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (inet_addr_any, cfg.port);
  listen sock cfg.backlog;
  Printf.printf "listening on port %d - backlog %d\n%!" cfg.port cfg.backlog;
  let rec go () =
    let client, _ = accept sock in
    S.fork cfg.handler client;
    go ()
  in
  go ()

let echo_handler client =
  let open Unix in
  let cleanup () = close client in
  let rec go () =
    let req = Socket.readline client in
    print_endline req;
    Socket.writeline client req;
    S.yield ();
    go ()
  in
  try Fun.protect ~finally:cleanup go
  with exn -> Printexc.to_string exn |> print_endline

let () =
  S.run @@ fun () ->
  server { port = 3000; backlog = 128; handler = echo_handler }
