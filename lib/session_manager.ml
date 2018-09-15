open Util
open Message
open Lwt.Infix

open Session



module Make (T: S.Transport): S.SessionManager = struct


  type callback = (Session.t -> unit Lwt.t)
  type state = Open | Draining | Closed

  module Transport = T

  module Flow = T.Flow

  

  type t = {
    transport: T.conn;
    mutable counter: int;
    mutable state: state;
    buffer_size: int; 

    sessions: (int32, Session.t) Tbl.t;
    outbox: Message.t Lwt_queue.t 
  }
  



  let establish t i (cb: callback) =
    let rep = `RINIT i in

    let tag =  Message.INIT.tag i in
    let inbox = Lwt_queue.create ()  in


    let s =
      Session.Private.create ~tag ~inbox ~outbox:t.outbox ()
      |> Session.Private.establish
    in

    Tbl.add t.sessions tag s; 


    Lwt_queue.put t.outbox rep >>= fun _ ->
    let _ = cb s in 

    Lwt.return_unit



  let close_session t tag =
    let ib = Tbl.find t.sessions tag in
    if Session.is_closed ib then 

      let _ = Tbl.remove_all t.sessions tag in 
      Session.Private.close ib >>= fun _ ->

      t.counter <- (t.counter - 1);
      Lwt.return_unit

    else
      Lwt.return_unit 



  
  


  let process t cb =
    T.read t.transport >>= function

    | `RPING tag ->
      let ib = Tbl.find t.sessions tag in
      Lwt_queue.put ib.inbox (`RPING tag)


    | `TPING tag ->
      Lwt_queue.put t.outbox (`RPING tag)


    | `TCLOSE tag ->
      close_session t tag >>= fun _ -> 
      Lwt_queue.put t.outbox (`RCLOSE tag)


    | `RINIT i ->
      t.counter <- t.counter + 1 ; 
      let s = Tbl.find t.sessions (INIT.tag i) in
      let _ = Session.Private.establish s in
      Lwt.return_unit


    | `TINIT i ->
      t.counter <- (t.counter + 1); 
      establish t i cb



    | `RCLOSE tag ->
      close_session t tag >>= fun _ -> 
      Lwt.return_unit


    | `TSHUTDOWN ->
      t.state <- Draining;
      Lwt.return_unit

    | `RSHUTDOWN ->
      t.state <- Closed;
      Lwt.return_unit



    | msg ->
      let tag = Message.tag msg in 
      let chan = Tbl.find t.sessions tag in

      Lwt_queue.put chan.inbox msg 




  let read_loop t ?cb () =

    let svc =
      match cb with
      | Some cb -> cb
      | None -> 
        (fun s ->
           Session.Private.close s >>= fun _ ->
           Lwt.return_unit 
        )
        
       
    in


    let rec aux () = 
      match t.state with
      | Open ->
        process t svc >>= fun _ ->
        aux ()

      | Draining when (t.counter >= 0) ->
        t.state <- Closed;
        aux ()

      | Draining ->
        process t svc >>= fun _ ->
        aux ()

      | Closed ->
        T.write t.transport `RSHUTDOWN >>= fun _ -> 
        T.close t.transport


    in

    aux ()

  let dispatch t =
    Lwt_queue.poll t.outbox  >>= fun msg ->
    T.write t.transport msg 



  let rec write_loop t =
    match t.state with

    | Closed -> Lwt.return_unit

    | _ -> dispatch t



  let write t msg =
    Lwt_queue.put t.outbox msg

  
end 
