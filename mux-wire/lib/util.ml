open Lwt.Infix
       
let handle_write flow w close =
  w flow >>= fun r ->
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> close flow >>= fun () -> Lwt.fail_with "connection error occured" 

module LFP (F: Mirage_flow_lwt.S) = struct

  open Buf
         
  let write flow buf =
    let size = Buf.len buf |> Int32.of_int in
    let len_pfx = Cstruct.create 8 in
    Cstruct.BE.set_uint32 len_pfx 0 size;

    handle_write flow (fun f -> F.write f len_pfx) F.close >>= fun () ->
    handle_write flow (fun f -> F.write f buf.buffer) F.close 

  let rec read flow size buf =
    if (Buf.len buf) >= size then 
      F.read flow >>= fun res ->

      match res with
      | Ok `Eof -> Error "connection closed" |> Lwt.return
      | Ok (`Data d) ->
         Buf.append buf d;
         read flow size buf
                     
      | Error e -> Error "Unknown error" |> Lwt.return

    else
      Lwt.return (Ok () )
end 


(*


let map_result r f =
  match r with
  | Ok res -> Ok (f res )
  | Error e -> Error e


let fmap_result r f =
  match r with
  | Ok res -> f res
  | Error e -> Error "failiure"

                     
let get_result r =
  match r with
  | Ok x -> x
  | Error _ -> Invalid_argument "result returns error"
  

let result_ok r =
  match r with
  | Ok _ -> true
  | Error _ -> false 



*)
