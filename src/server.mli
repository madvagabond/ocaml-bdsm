open Messages

(** 
   The reason why Listeners are used instead of stacks is because the original scala implementation was transport Generic, and ran on UDT by default
*)
module type Listener = sig
  include Mirage_flow_lwt.S

            
  type callback = flow -> unit Lwt.t
  val listen: port:int -> callback -> unit Lwt.t 
end 


                         

(** 
   The reason why Listeners are used instead of stacks is because the original scala implementation was transport Generic, and ran on UDT by default
*)
module S:
functor (L: Listener) -> sig
  type handler = L.flow -> TMSG.t -> unit Lwt.t

  val respond: L.flow -> RMSG.t -> unit Lwt.t
  val recv: L.flow -> TMSG.t Lwt.t 

                                                                                  
  val make_handler: cb: (TMSG.t -> RMSG.t Lwt.t) -> unit -> handler
  val listen: port:int -> handler: handler -> unit -> unit Lwt.t
                                                             
end
