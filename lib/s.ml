


module type Peer = sig
  type t
    
  val host: t -> string
  val port: t -> int
  val id: t -> int32 
end




module type IO = sig
  type conn
  type peer
    
  val read_exactly: int  -> conn-> Cstruct.t Lwt.t
  val write_buffer: conn -> Cstruct.t -> unit Lwt.t

  val close: conn -> unit Lwt.t
  val dst: peer
end





module type Transport = sig
  type conn

  module IO: IO with type conn := conn

  
  val read: conn -> Message.t Lwt.t
  val write: conn -> Message.t -> unit Lwt.t
  val close: conn -> unit Lwt.t

  
end






module type Connector = sig
  include Transport
  type peer
    

  val connect: peer -> conn Lwt.t
end






module type Listener = sig
  type peer
  include Transport with type IO.peer = peer
  
  val listen: IO.peer -> (conn -> unit Lwt.t) -> unit Lwt.t
end 









(** Base protocol implementation. *)
module type Protocol = sig
  type t



  val handle_init: t -> Message.init -> unit Lwt.t
  val handle_req: t -> Message.req -> (Session.t -> unit Lwt.t) -> unit Lwt.t

  
  val handle_close: t -> Message.close -> unit Lwt.t
  val handle_shutdown: t -> Message.shutdown -> unit Lwt.t
  val handle_err: t -> Message.rerror -> unit Lwt.t


  val drained: t -> bool 

  val closed: t -> Session.t -> bool
  val close: t -> Session.t -> unit Lwt.t 
  
  val read: t -> Message.t Lwt.t 
  val write: t -> Message.t -> unit Lwt.t

  
end







module type SessionManager = sig

  type t
  type conn

  module Transport: Transport with type conn = conn
                                     

  
  val process: t -> (Session.t -> unit Lwt.t) -> unit Lwt.t


  val read_loop: t -> ?cb: (Session.t -> unit Lwt.t) -> unit -> unit Lwt.t
  val write_loop: t -> unit Lwt.t

  val write: t -> Message.t -> unit Lwt.t
  val create: conn -> t

  val shutdown: t -> unit Lwt.t
  val ping: t -> Message.rping Lwt.t

  val create_session: t -> Session.t Lwt.t
      
      
end











(** 
  Both clients and servers are this, their differences are in how they satisfy the signature. 
*)
module type Endpoint = sig

  type t
  type peer

  type conn 
  
  module SM: SessionManager
 

  val create_session: t -> peer -> Session.t Lwt.t


  
  val serve: t -> peer -> cb: (Session.t -> unit Lwt.t) -> unit -> unit Lwt.t
  val shutdown: t -> peer -> unit Lwt.t
      
  val ping: t -> peer -> Message.rping Lwt.t


  
end


