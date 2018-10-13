open Message


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

  (** Remote address *)
  val dst: conn -> peer

  
end



(** A typed bidirectional stream *)
module type Transport = sig
  type conn
  type peer
    

  
  val read: conn -> Message.t Lwt.t
  val write: conn -> Message.t -> unit Lwt.t
  val close: conn -> unit Lwt.t

  (** Remote address *)
  val dst: conn -> peer

 
  
end


module type Connector = sig
  include Transport

  val connect: peer -> conn Lwt.t
end






module type Listener = sig
  include Transport
  val listen: peer -> (conn -> unit Lwt.t) -> unit Lwt.t
end 




(** Manages sessions on top of physical connections. *)
module type SessionManager = sig

  type t
  type conn
  type peer
    

  module Transport: Transport with type conn := conn and type peer := peer
                                     


  val read_loop: t -> ?cb:(Session.t -> unit Lwt.t) -> unit -> unit Lwt.t

  val write_loop: t -> unit Lwt.t
  
  val process: t -> (Session.t -> unit Lwt.t) -> unit Lwt.t
  val write: t -> Message.t -> unit Lwt.t
  val create: conn -> t

  val shutdown: t -> unit Lwt.t
  val ping: t -> Message.rping Lwt.t

  val create_session: t -> Session.t Lwt.t
  val dst: t -> peer


  val transport: t -> conn
      
      
end


module type Client = sig
  type t

  type peer
  type conn

  module SM: SessionManager with type conn := conn and type peer := peer
    

  (** Establishes connection to server *)
  val create: peer -> t Lwt.t

  val shutdown: t -> unit Lwt.t
  val dispatch: t -> TMSG.t -> RMSG.t Lwt.t

  (** Creates a session *)
  val create_session: t -> Session.t Lwt.t
  val ping: t -> Message.rping Lwt.t

  (** Remote address *)
  val dst: t -> peer


end



module type Server = sig

  type t
  type peer

  type conn 
  
  module SM: SessionManager with type conn := conn and type peer := peer

  val serve: peer -> (Session.t -> unit Lwt.t) -> t Lwt.t
  val ping: t -> peer -> Message.rping Lwt.t

  val shutdown: t -> peer -> unit Lwt.t
  val serve_dispatch: peer -> (TMSG.t -> RMSG.t Lwt.t) -> t Lwt.t
  val create_session: t -> peer -> Session.t Lwt.t
                                    
  
end


