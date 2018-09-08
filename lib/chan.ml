open Lwt.Infix
open Util
       
module Make (FLOW: Mirage_flow_lwt.S) = struct
  module IO = Mirage_channel_lwt.Make(FLOW)

  type conn = IO.t
                


  let read_exactly ~len c =
    IO.read_exactly ~len c >>= function
    | Ok (`Data bufs) -> Lwt.return (Ok (Cstruct.concat bufs) )
    | Ok `Eof ->
      IO.close c >>= fun _ ->
      Lwt.return (Error "connection closed")

    | Error e -> Lwt.return (Error (Fmt.strf "%a" IO.pp_error e))


                            
                                     
  let read c =
    
    read_exactly 4 c >>= function
    | Ok x ->
      begin 
       let len = Cstruct.BE.get_uint32 x 0 |> Int32.to_int in
       read_exactly len c >>= function
       | Ok buf -> Ok (Cstruct.append x buf) |> Lwt.return 
       | Error d -> Lwt.return (Error d)
     end

     
                  
    | err -> Lwt.return (err)
                  


  let read_message c =
    read c >|= function 
    | Ok buf -> Util.map_result (Frame.of_cstruct buf) Messages.of_frame
    | Error data -> Error "unable to read frame"
               
      
    
  
  let write c data =
    IO.write_buffer c data



                              

                              
                              
                    
              
end 
