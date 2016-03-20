(** This module defines the code coverage line information output to a CSV *)

val output
  : verbose:(string -> unit)
  -> out_file:string
  -> tab_size:int
  -> resolver:(string -> string option)
  -> data:(string, int array) Hashtbl.t
  -> unit
(** [output ~verbose ~out_file ~tab_size ~title ~resolver ~data] writes all code
    coverage line information for [data] to the [out_file] in comma separated
    format. [verbose] is used for verbose output, [tab_size] is the number of
    space characters to use as a replacement for tabulations, and [resolver]
    associates actual paths to given filenames. *)







