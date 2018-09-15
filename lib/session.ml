open Lwt.Infix 

exception Session_Closed
type status = Negotiating | Closed | Open

type t = {
  mutable status: status;
  tag: int32; 
  inbox: Message.t Lwt_queue.t;
  outbox: Message.t Lwt_queue.t 
}


(** Used by Session Manager *)
module Private = struct

  let close t =
    t.status <- Closed;
    Lwt_queue.close t.inbox >|= fun _ ->
    t

  let establish t =
    t.status <- Open;
    t


  let create ?(tag=Message.random_tag ()) ~inbox ~outbox () =
    let status = Negotiating in
    {inbox; outbox; status; tag}


end 




let rec recv t =
  match t.status with

  | Negotiating ->
    recv t
  
  | Closed when (Lwt_queue.size t.inbox) <= 0 ->
    Lwt.fail Session_Closed

  | _ ->
    Lwt_queue.poll t.inbox



let rec send msg t =
  match t.status with
  | Negotiating -> send msg t 
  | Closed -> Lwt.fail Session_Closed
  | Open -> Lwt_queue.put t.outbox msg



let close t =
  Private.close t >>= fun _ ->
  Lwt_queue.put t.outbox (`TCLOSE t.tag) 
  

let tag t = t.tag


module Infix = struct
  (** send *)
  let (!) msg t = send msg t



  (** recv *)
  let (<:) t = recv t

end

include Infix


    
let status t =
  t.status

let is_closed t =
  match t.status with
  | Closed -> true
  | _ -> false
