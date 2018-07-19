
module S:
functor (Flow: Mirage_flow_lwt.S) -> sig
  
  val send: Flow.flow -> Messages.TMSG.t -> unit Lwt.t 
  val recv: Flow.flow -> Messages.RMSG.t Lwt.t
  val send_recv: Flow.flow -> Messages.TMSG.t -> Messages.RMSG.t Lwt.t
end 


                                       
