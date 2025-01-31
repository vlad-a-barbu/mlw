val read : ?stop_cond:(string -> bool) -> Unix.file_descr -> string
val write : Unix.file_descr -> string -> unit
