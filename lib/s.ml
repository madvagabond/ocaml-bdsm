
module type LabelledFlow = sig
  include Mirage_flow_lwt.S

  type peer
  val dst: flow -> peer  
end 



module type Transport = sig
  type conn

  module Flow: LabelledFlow with type flow = conn

  
  val read: conn -> Message.t Lwt.t
  val write: conn -> Message.t -> unit Lwt.t
  val close: conn -> unit Lwt.t

  
end

module type Connection_Manager = sig
  include Transport

  val connect: Flow.peer -> conn Lwt.t
end


module type Client = sig
  type t
  type peer

 
  val create_session: t -> peer -> Session.t Lwt.t
      
  val ping: t -> peer -> Message.rping Lwt.t
  val shutdown: t -> peer -> Session.t Lwt.t
end



module type Listener = sig
  include Transport  
  val listen: string -> int -> (conn -> unit Lwt.t) -> unit Lwt.t
end 





module type Server = sig

  type t
  type peer

  
  module Transport: Transport 
    
  val serve: string -> int -> cb: (Session.t -> unit Lwt.t) -> unit -> unit Lwt.t
  val shutdown: t -> peer -> unit Lwt.t
      
  val ping: t -> peer -> Message.rping Lwt.t
      
end


module type SessionManager = sig
  type t
  
  module Flow: LabelledFlow

  module Transport: Transport with type conn = Flow.flow


  val process: t -> (Session.t -> unit Lwt.t) -> unit Lwt.t


  val read_loop: t -> ?cb: (Session.t -> unit Lwt.t) -> unit -> unit Lwt.t
  val write_loop: t -> unit Lwt.t

  val write: t -> Message.t -> unit Lwt.t
  
end
