
open Lwt.Infix
       
module Reader = Codec.Reader
open Messages
       
                  
module Make (Flow:  Mirage_flow_lwt.S) = struct

  
  module R = Reader(Flow)(RMSG)


  let to_lwt res =
    match res with
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail_with e
                   

  let send flow tmsg =
    let buf = Messages.TMSG.encode tmsg in
    Flow.write flow buf >>= fun res ->

    match res with
    | Ok r -> Lwt.return_unit

    | Error e ->
       let estring =
         Fmt.strf "%a" Flow.pp_write_error e
       in
       Lwt.fail_with estring

                     

  let recv flow =
    R.read flow >>= fun rep ->
    to_lwt rep


  let send_recv flow req =
    send flow req >>= fun _ ->
    recv flow 
    
end
