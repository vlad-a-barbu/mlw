open Effect
open Effect.Deep

type 'a Effect.t +=
  | Fork : (('a -> unit) * 'a) -> unit Effect.t
  | Yield : unit Effect.t

let fork fn arg = perform @@ Fork (fn, arg)
let yield () = perform Yield

let run prog =
  let q = Queue.create () in
  let enqueue k = Queue.push (fun () -> continue k ()) q in
  let dequeue () = if Queue.is_empty q then () else Queue.pop q () in
  let rec step fn =
    match fn () with
    | () -> dequeue ()
    | exception exn ->
        Printexc.to_string exn |> print_endline;
        dequeue ()
    | effect Yield, k ->
        enqueue k;
        dequeue ()
    | effect Fork (fn, arg), k ->
        enqueue k;
        step @@ fun () -> fn arg
  in
  step prog
