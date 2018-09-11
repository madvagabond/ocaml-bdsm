open Util
open Lwt.Infix
open Message

open Session
 
module SessionManager (T: Transport.S) = struct


  type updates = (Message.t option -> unit)

  type state = Open | Draining | Closed
               

  type t = {
    transport: T.conn;
    mutable counter: int;
    mutable state: state;
    buffer_size: int; 

    sessions: (int32, Session.t) Tbl.t;
    outbox: Message.t Lwt_queue.t 
  }


  


  let rping = `RPING 0l
  
(*
  let process t msg =
    let tag = Message.tag msg in 

    Sessions.map t.sessions tag >>= fun f -> 
    let _ = f (Some msg) in
    
    Lwt.return_unit



  let no_service msg =
    let open RREQ in 
    let tag = Message.tag msg in
    let status = Status.to_int `Nack in
    let headers = Headers.init () in

    let body = Cstruct.of_string "client doesn't serve requests" in 
    `RREQ {tag; status; headers; body}
*)







  let process t =
    T.read t.transport >>= function

    | `TPING 0l ->
      Lwt_queue.put t.outbox rping


    | `RPING 0l ->
      let ib = Tbl.find t.sessions 0l in
      Lwt_queue.put ib.inbox (`RPING 0l)

  
    | `TCLOSE tag ->
      let ib = Tbl.find t.sessions tag in 
      Tbl.remove_all t.sessions tag;
      close ib >>= fun _ -> 

      Lwt_queue.put t.outbox (`RCLOSE tag)

  
    | `RINIT i ->
      let s = Tbl.find t.sessions (INIT.tag i) in
      let _ = Session.Private.establish s in
      Lwt.return_unit
        


    | `RCLOSE tag ->
      let ib = Tbl.find t.sessions tag in
      Tbl.remove_all t.sessions tag;
      Session.Private.close ib >>= fun _ -> 

      t.counter <- (t.counter - 1);
      Lwt.return_unit



    | `TSHUTDOWN ->
      t.state <- Draining;
      Lwt.return_unit

      
    | msg ->
      let tag = Message.tag msg in 
      let chan = Tbl.find t.sessions tag in
      
      Lwt_queue.put chan.inbox msg 
      
  


  let rec read_loop t =
    match t.state with
    | Open ->
      process t >>= fun _ ->
      read_loop t

    | Draining when (t.counter >= 0) ->
      t.state <- Closed;
      read_loop t

    | Draining ->
      process t >>= fun _ ->
      read_loop t
        
    | Closed ->
      T.close t.transport




  let dispatch t =
    Lwt_queue.poll t.outbox  >>= fun msg ->
    T.write t.transport msg 

  
  
  let rec write_loop t =
    match t.state with
    | Open ->
      dispatch t

    | _ ->
      Lwt.return_unit




  let create_session t =
    let open INIT in
    
    let sesh =
      Session.Private.create ~inbox:(Lwt_queue.create () ) ~outbox:t.outbox ()
    in

    
    let tag = Session.tag sesh in
    let init = {tag;headers = Headers.init ()} in
    let msg = (`TINIT init) in

    
    
    Tbl.add t.sessions tag sesh; 
    Lwt_queue.put t.outbox msg
      


 
           

             
end 
  (*

      | Ok (`TPING tag) ->
        t.send (Some rping);
        aux ()

      | Ok (`RCLOSE tag) ->
        Inbox.shutdown t.inbox tag >>= fun _ ->
        aux ()

      | Ok (`RINIT init) ->
        process t (`RINIT init) >>= fun _ ->
        aux () 
        
        
      | Ok (`TINIT init) ->
        process t (`RINIT init) >>= fun _ ->
        aux () 
        
        
  
      | Ok (`RPING tag) ->
        process t (`RPING tag) >>= fun _ ->
        aux ()

     

      | Ok (`TCLOSE tag) ->
        Inbox.unmap t.inbox tag >>= fun _ ->
        t.send ( Some (`RCLOSE tag) );
        aux ()
          
        
        

*)
