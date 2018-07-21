open Messages
       
module Make:
functor (Flow: Mirage_flow_lwt.S) -> sig
  
  val send: Flow.flow -> TMSG.t -> unit Lwt.t 
  val recv: Flow.flow -> RMSG.t Lwt.t
  val send_recv: Flow.flow -> TMSG.t -> RMSG.t Lwt.t
end 


                                       
