module S = Mlw.Scheduler

let rec countdown n =
  if n = 0 then ()
  else (
    n |> string_of_int |> print_endline;
    S.yield ();
    countdown (n - 1))

let () =
  S.run @@ fun () ->
  S.fork countdown 3;
  S.fork countdown 3
