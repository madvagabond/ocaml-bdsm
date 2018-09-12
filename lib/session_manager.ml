open Util
open Message
open Lwt.Infix

open Session




module Client (T: Transport.S) = struct


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



  let process t =
    T.read t.transport >>= function

    | `RPING tag ->
      let ib = Tbl.find t.sessions tag in
      Lwt_queue.put ib.inbox (`RPING tag)
        
      
    | `TPING tag ->
      Lwt_queue.put t.outbox (`RPING tag)
        

    | `TCLOSE tag ->
      let ib = Tbl.find t.sessions tag in 
      Tbl.remove_all t.sessions tag;
      t.counter <- (t.counter - 1);
      
      close ib >>= fun _ -> 

      Lwt_queue.put t.outbox (`RCLOSE tag)


    | `RINIT i ->
      t.counter <- t.counter + 1 ; 
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

    | `RSHUTDOWN ->
      t.state <- Closed;
      Lwt.return_unit

    | `TINIT _ -> Lwt.return_unit 

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
      T.write t.transport `RSHUTDOWN >>= fun _ -> 
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



module Server (T: Transport.S) = struct
  


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



  type 'a callback = Session.t -> 'a Lwt.t

  let establish t i cb=
    let rep = `RINIT i in

    let tag =  Message.INIT.tag i in
    let inbox = Lwt_queue.create ()  in


    let s =
      Session.Private.create ~tag ~inbox ~outbox:t.outbox ()
      |> Session.Private.establish
    in

    Tbl.add t.sessions tag s; 


    Lwt_queue.put t.outbox rep >>= fun _ ->
    Lwt.async ( cb s );

    Lwt.return_unit

    


  let process t cb =
    T.read t.transport >>= function
    | `RPING tag ->
      let ib = Tbl.find t.sessions tag in
      Lwt_queue.put ib.inbox (`RPING tag)


    | `TPING tag ->
      Lwt_queue.put t.outbox (`RPING tag)

    | `TINIT i ->
      t.counter <- (t.counter + 1); 
      establish t i cb


    | `TCLOSE tag ->
      let ib = Tbl.find t.sessions tag in 
      Tbl.remove_all t.sessions tag;
      t.counter <- (t.counter - 1);

      close ib >>= fun _ -> 

      Lwt_queue.put t.outbox (`RCLOSE tag)



    | `RCLOSE tag ->
      let ib = Tbl.find t.sessions tag in
      Tbl.remove_all t.sessions tag;
      Session.Private.close ib >>= fun _ -> 

      t.counter <- (t.counter - 1);
      Lwt.return_unit




    | `TSHUTDOWN ->
      t.state <- Draining;
      Lwt.return_unit

  
    | `RSHUTDOWN ->
      t.state <- Closed;
      Lwt.return_unit


    | `RINIT _ -> Lwt.return_unit
                    
  
    | x ->
      let tag = Message.tag x in  
      let s = Tbl.find t.sessions tag in
      Lwt_queue.put s.inbox x 
      
      



  let dispatch t =
    Lwt_queue.poll t.outbox  >>= fun msg ->
    T.write t.transport msg 


  let rec write_loop t =
    match t.state with
    | Closed ->
      Lwt.return_unit 

    | _ ->
      dispatch t >>= fun _ ->
      write_loop t


  


  let rec read_loop t cb =
    match t.state with
    | Open ->
      process t cb >>= fun _ ->
      read_loop t cb

    | Draining when (t.counter >= 0) ->
      t.state <- Closed;
      read_loop t cb

    | Draining ->
      process t cb >>= fun _ ->
      read_loop t cb

    | Closed ->
      T.write t.transport `RSHUTDOWN >>= fun _ -> 
      T.close t.transport




  

      
 


end  
