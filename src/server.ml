open Lwt.Infix
open Messages
       

                
module type Listener = sig
  include Mirage_flow_lwt.S

            
  type callback = flow -> unit Lwt.t
  val listen: port: int -> callback -> unit Lwt.t 
end 





module Make (L: Listener) = struct
  type handler = L.flow -> TMSG.t  -> unit Lwt.t



  module R = Codec.Reader(L)(TMSG)

  let to_lwt res =
    match res with
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail_with e
                               
                               

  let recv flow =
    R.read flow >>= fun res ->
    to_lwt res
           

  let respond flow rep =
    let buf = RMSG.encode rep in
    L.write flow buf >>= fun res ->
    match res with
      
    | Ok r -> Lwt.return_unit

    | Error e ->
       
       let estring =
         Fmt.strf "%a" L.pp_write_error e
       in
       
       Lwt.fail_with estring


                     
                     
                                              
  let make_handler ~cb () =


    let handler flow tmsg  =
      cb tmsg >>= fun rep ->
      respond flow rep
    in

    handler 



      
  let listen ~port ~handler () =

    let callback flow =
      recv flow >>= fun req ->
      handler flow req
    in

    L.listen ~port callback
      
end 
