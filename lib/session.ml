open Lwt.Infix 
open Message


exception Session_Closed


let dispatch_err = Cstruct.of_string "Did not recv RMSG"
    
type status = Closed | Open

type t = {
  mutable status: status;
  tag: int32; 
  inbox: Message.msg Lwt_queue.t;
  outbox: Message.t Lwt_queue.t;
  
 
}

let create ?(tag=Message.random_tag ()) ?(inbox=Lwt_queue.create ()) ~outbox () =
  
  {status = Open; inbox; outbox; tag}



let recv t =
  Lwt_queue.poll t.inbox


let is_closed t =
  match t.status with
  | Open -> false 
  | Closed -> true


let rec send msg t =
  Lwt_queue.put t.outbox msg






module Infix = struct
  (** send *)
  let (!) msg t = send msg t



  (** recv *)
  let (<:) t = recv t

end

include Infix


    
(*
let is_closed t =
  match t.status with
  | Closed -> true
  | _ -> false
*)

let mark_closed t =
  t.status <- Closed

let close t =
  t.status <- Closed;
  send (`TCLOSE t.tag) t
  



let dispatch t msg =
  send (`TMSG {msg with tag = t.tag}) t  >>= fun () ->
  recv t >>= function
  | `RMSG rep ->
    close t >>= fun () -> 
    Lwt.return rep
  | _ ->
    let body = dispatch_err in 
    send ( `RERROR (RERROR.create ~tag: msg.tag ~body () ) ) t >>= fun _ ->
    close t >>= fun () ->
    Lwt.fail_with "Protocol Error"



let serve_dispatch t cb =
  recv t >>= function

  | `TMSG req ->
    cb req >>= fun rep ->
    send (`RMSG {rep with tag = t.tag} ) t >>= fun () ->
    close t 

  | `RMSG rep -> 
    let body = dispatch_err in 
    send ( `RERROR (RERROR.create ~tag: rep.tag ~body () ) ) t >>= fun _ ->
    close t >>= fun () ->
    Lwt.fail_with "Protocol Error"
 
