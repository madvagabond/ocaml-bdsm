open Util
open Message
open Lwt.Infix

open Session
open Message


module Make (T: S.Transport): S.SessionManager with type conn = T.conn and type peer = T.peer = struct


  let not_est = Cstruct.of_string "Stream Not Established"

  type callback = (Session.t -> unit Lwt.t)
  type state = Open | Draining | Closed

  module Transport = T

  type conn = T.conn
  type peer = T.peer
                

  type t = {
    transport: T.conn;
    
    mutable counter: int;
    mutable state: state;

    sessions: (int32, Session.t) Tbl.t;
    pending: (int32, Session.t Lwt.u) Tbl.t; 
    outbox: Message.t Lwt_queue.t;
    pings: Message.rping Lwt_queue.t 
             
  }
  



  let handle_tinit t i (cb: callback) =
    let rep = `RINIT i in

    t.counter <- (t.counter + 1); 
    let tag =  Message.INIT.tag i in

    let s =
      Session.create ~tag ~outbox:t.outbox ()
    in

    Tbl.add t.sessions tag s; 


    Lwt_queue.put t.outbox rep >>= fun _ ->
    let _ = cb s in 

    Lwt.return_unit



  
  let handle_rinit t tag =
    let pending = Tbl.find t.pending tag in
    t.counter <- t.counter + 1 ;

    Lwt.wakeup pending (Session.create ~tag ~outbox: t.outbox () );
    Lwt.return_unit

  let close_session t tag =
    let ib = Tbl.find_opt t.sessions tag in
    match ib with
    | Some ib -> 

      let _ = Tbl.remove_all t.sessions tag in 
      Session.mark_closed ib;

      t.counter <- (t.counter - 1);
      Lwt.return_unit

    | None ->
      Lwt.return_unit
        





  let handle_req t (req: Message.msg) =
    let tag = Message.tag req in
    let s = Tbl.find_opt t.sessions tag in
    match s with
    | Some s ->
      Lwt_queue.put s.inbox req
    | None ->
      let rep = `RERROR (RERROR.create ~tag ~body: not_est ()) in
      Lwt_queue.put t.outbox rep
  


  let process t cb =
    T.read t.transport >>= function

    | `RPING ->
      Lwt_queue.put t.pings `RPING


    | `TPING  ->
      Lwt_queue.put t.outbox `RPING


    | `TCLOSE tag ->
      close_session t tag >>= fun _ -> 
      Lwt_queue.put t.outbox (`RCLOSE tag)


    | `RINIT i ->
      handle_rinit t (INIT.tag i)
        

    | `TINIT i ->
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



    | `TMSG req -> handle_req t (`TMSG req)

    | `RMSG req -> handle_req t (`RMSG req)

    | `RERROR d ->
      let s = Tbl.find t.sessions (RERROR.tag d) in
      Session.close s

  
      




  let read_loop t ?cb () =

    let svc =
      match cb with
      | Some cb -> cb
      | None -> 
        (fun s ->
           Session.close s >>= fun _ ->
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

    | _ ->
      dispatch t >>= fun _ ->
      write_loop t 
    



  let write t msg =
    Lwt_queue.put t.outbox msg


  let create conn =

    let transport = conn in
    let counter = 0 in
    let state = Open in
    
    let sessions = Hashtbl.create 100 in
    let outbox =  Lwt_queue.create () in
    let pings = Lwt_queue.create () in
    let pending = Hashtbl.create 1024 in

    {
      transport; counter; state;
      pending;
      outbox; pings; sessions
    }

  

  
  let ping t =
    write t `TPING >>= fun _ ->
    Lwt_queue.poll t.pings

  let shutdown t =
    t.state <- Draining;
    write t `TSHUTDOWN 


 

  
  let create_session t =
  
    let req = INIT.create () in
    let (p, u) = Lwt.wait () in
    let tag = INIT.tag req in 
    Tbl.add t.pending tag u ; 
    
    write t (`TINIT req) >>= fun _ ->
    p >|= fun s ->
    Tbl.remove_all t.pending tag;
    Tbl.add t.sessions (INIT.tag req) s;
    s


  let dst t =
    Transport.dst t.transport

  let transport t = t.transport
  
end 
