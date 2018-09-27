open Util
open Message
open Lwt.Infix

open Session



module Make (T: S.Transport): S.SessionManager with type conn = T.conn = struct


  type callback = (Session.t -> unit Lwt.t)
  type state = Open | Draining | Closed

  module Transport = T

  type conn = T.conn

  type t = {
    transport: conn;
    
    mutable counter: int;
    mutable state: state;

    sessions: (int32, Session.t) Tbl.t;
    outbox: Message.t Lwt_queue.t;
    pings: Message.rping Lwt_queue.t 
             
  }
  



  let handle_tinit t i (cb: callback) =
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





  let handle_req t req =
    let tag = Message.tag req in
    let s = Tbl.find t.sessions tag in
    Lwt_queue.put s.inbox req  
  
  


  let process t cb =
    T.read t.transport >>= function

    | `RPING ->
      Lwt_queue.put t.pings `RPING


    | `TPING  ->
      Lwt_queue.put t.outbox `TPING


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
      handle_tinit t i cb



    | `RCLOSE tag ->
      close_session t tag >>= fun _ -> 
      Lwt.return_unit


    | `TSHUTDOWN ->
      t.state <- Draining;
      Lwt.return_unit

    | `RSHUTDOWN ->
      t.state <- Closed;
      Lwt.return_unit



    | `TREQ req -> handle_req t (`TREQ req)

    | `RREQ req -> handle_req t (`RREQ req)

    | `RERROR d ->
      let s = Tbl.find t.sessions (RERROR.tag d) in
      Session.close s

  
      




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


  let create conn =

    let transport = conn in
    let counter = 0 in
    let state = Open in
    
    let sessions = Hashtbl.create 100 in
    let outbox =  Lwt_queue.create () in
    let pings = Lwt_queue.create () in

    {
      transport; counter; state;
      outbox; pings; sessions
    }

  

  
  let ping t =
    write t `TPING >>= fun _ ->
    Lwt_queue.poll t.pings

  let shutdown t =
    t.state <- Draining;
    write t `TSHUTDOWN 



  
  let create_session t =
  
    let inbox = Lwt_queue.create () in 
    let session = Session.Private.create ~inbox ~outbox: t.outbox () in
    let req = INIT.create ~tag: (Session.tag session) () in
    
    write t (`TINIT req) >|= fun _ ->

    Tbl.add t.sessions (session.tag) session ; 
    session
      
  
end 
