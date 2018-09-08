open Util
open Lwt.Infix
open Messages
    
       
    
module SessionManager (F: Mirage_flow_lwt.S) = struct
  module Chan = Chan.Make(F)


  type updates = (Messages.t option -> unit)

  type t = {
    chan: Chan.conn;
    inbox: updates Inbox.t;
    outbox: updates 
  }


  let rping = `RPING 0l
  

  let process t msg =
    let tag = Messages.tag msg in 
    Inbox.map t.inbox tag >>=
    function
    | Ok f ->
      let _ = f (Some msg) in
      Lwt.return_unit

    | Error e ->
      Chan.IO.close t.chan >>= fun _ -> 
      Lwt.fail_with "session doesn't exist"


  let no_service msg =
    let open RREQ in 
    let tag = Messages.tag msg in
    let status = Status.to_int `Nack in
    let headers = Headers.init () in

    let body = Cstruct.of_string "client doesn't serve requests" in 
    `RREQ {tag; status; headers; body}

  

  let read_loop t ?svc () =



    let service =
      let open RREQ in

      match svc with
      | Some x -> x
      | None -> no_service

    in

    
    let rec aux () = 

      Chan.read_message t.chan >>= fun msg ->
      match msg with

      | Ok (`RREQ m) ->
        process t (`RREQ m) >>= fun _ ->
        aux () 


      | Ok (`TREQ m) ->
        let res = service (`TREQ m) in
        let _ = t.outbox (Some res) in
        aux ()

      | Ok (`TPING tag) ->
        t.outbox (Some rping);
        aux ()

      | Ok (`RCLOSE tag) ->
        Inbox.shutdown t.inbox tag >>= fun _ ->
        aux ()

      | Ok (`RINIT init) ->
        process t (`RINIT init) >>= fun _ ->
        
        
  
      | Ok (`RPING tag) ->
        process t (`RPING tag) >>= fun _ ->
        aux ()
        
        


      | Error e ->
        Chan.IO.close t.chan >>= fun _ ->
        Lwt.fail_with "Error occured in read loop, so connection was closed"


    in

    
    aux () 


  
           
            
             
end 
