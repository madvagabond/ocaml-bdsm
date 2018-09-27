open Lwt.Infix 
open Message


exception Session_Closed
type status = Negotiating | Closed | Open

type t = {
  mutable status: status;
  tag: int32;
  
  inbox: Message.req Lwt_queue.t;
  outbox: Message.t Lwt_queue.t;
  
 
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


let close t =
  Private.close t >>= fun _ ->
  send (`TCLOSE t.tag) t



let shutdown t =
  close t >>= fun _ ->
  send `TSHUTDOWN t 



module ReqRep = struct



  let nack t =
    let body = Cstruct.of_string "Wrong message type recieved" in
    `RREQ (RREQ.create ~tag: t.tag ~status:`Nack ~body () )
        
  module Sender = struct

    type res = (Message.RREQ.t, Message.treq) result




    let dispatch t tmsg =

      let req = `TREQ tmsg in
      send req t >>= fun _ ->
      recv t  >|= function
      | `RREQ data -> Ok data
      | `TREQ data -> Error data

  end



  module Receiver = struct

    let dispatch t cb = 
        recv t >>= function

        | `TREQ req ->
          cb req  >>= fun rep ->
          send (`RREQ rep) t
            
        | `RREQ _ ->
          send (nack t) t
    
      
  end 
  
end 
