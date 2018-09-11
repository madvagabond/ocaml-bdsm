open Lwt.Infix
open Util


module type S = sig
  type conn

  module FLOW: Mirage_flow_lwt.S
  
  val read: conn -> Message.t Lwt.t
  val write: conn -> Message.t -> unit Lwt.t
  val close: conn -> unit Lwt.t
      
end



module Make (FLOW: Mirage_flow_lwt.S): S = struct
  module IO = Mirage_channel_lwt.Make(FLOW)

  module FLOW = FLOW 
  type conn = IO.t
                


  let read_exactly ~len c =
    IO.read_exactly ~len c >>= function
    | Ok (`Data bufs) -> Lwt.return (Ok (Cstruct.concat bufs) )
    | Ok `Eof ->
      IO.close c >>= fun _ ->
      Lwt.return (Error "connection closed")

    | Error e -> Lwt.return (Error (Fmt.strf "%a" IO.pp_error e))




  let read_frame c =

    read_exactly 4 c >>= function
    | Ok x ->
      begin 
        let len = Cstruct.BE.get_uint32 x 0 |> Int32.to_int in
        read_exactly len c >>= function
        | Ok buf -> Ok (Cstruct.append x buf) |> Lwt.return 
        | Error d -> Lwt.return (Error d)
      end



    | err -> Lwt.return (err)



  let read c =
    read_frame c >>= function 
    | Ok buf ->
      Util.map_result (Frame.of_cstruct buf) Message.of_frame
      |> Util.get_result
      |> Lwt.return
           

    | Error data ->
      IO.close c >>= fun _ -> 
      Lwt.fail_with "Error connection closed"




  let write c msg =
    let data = Message.to_frame msg |> Frame.to_cstruct in 
    IO.write_buffer c data;


    IO.flush c >>= function
    | Ok () -> Lwt.return_unit
    | _ ->
      IO.close c >>= fun _ -> 
      Lwt.fail_with "Error connection closed"




                              
                              
                    
  let close c =
    IO.close c >>= fun _ ->
    Lwt.return_unit
      
end 
