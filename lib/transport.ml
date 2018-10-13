open Lwt.Infix
open Util





module Make (IO: S.IO) = struct
  module IO = IO

  type peer = IO.peer
  type conn = IO.conn
                


  let read_frame c =

    IO.read_exactly 4 c >>= fun x ->
    let len = Cstruct.BE.get_uint32 x 0 |> Int32.to_int in
    IO.read_exactly len c 







  let read c =
    read_frame c >>= fun buf ->
    Util.map_result (Frame.of_cstruct buf) Message.of_frame
    |> Util.get_result
    |> Lwt.return


  let write c msg =
    let data = Message.to_frame msg |> Frame.to_cstruct in 
    IO.write_buffer c data


                              
                              
                    
  let close c =
    IO.close c >>= fun _ ->
    Lwt.return_unit


  let dst c = IO.dst c
      
end 
