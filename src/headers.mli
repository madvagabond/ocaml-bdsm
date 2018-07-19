
type t = (string * string) list
       
val get: t -> string -> string option
val get_multi: t -> string -> string list

val add: t -> string -> string -> t
                                    
val add_multi: t -> string -> string list -> t
val add_list: t -> (string * string) list -> t 

val get_list: t -> string list -> (string * string) list
                                    
                                                 
val init : unit -> t

