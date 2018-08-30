open Buf
open Lwt.Infix
       
type 'a parse_out = [
    `Done of 'a |
    `Out_of_bounds of (int * 'a)
  ]
type t = (string * string) list

let write_kv buf kv =
  let (k, v) = kv in
  let k_l = String.length k in
  let v_l = String.length v in

  Buf.write_int16 buf k_l;
  write_string buf k;

  
  
  Buf.write_int16 buf v_l;
  write_string buf v
  

(* Modify to shift back on failure*)
let read_kv buf =
  let k = Buf.read_int16 buf |> Buf.read_string buf in
  let v = Buf.read_int16 buf |> Buf.read_string buf in
  k, v
       
       
       
                           
let encode t buf =
  let card = List.length t in
  write_int16 buf card;

  List.iter (fun kv ->
      write_kv buf kv 
    ) t
  



                                    
let decode ~ct buf =
  
  let rec aux c hdrs =
    if c > 0 then
      let c1 = c - 1 in 
      let kv = read_kv buf in

      aux c1 (hdrs @ [kv])
          
          

    else
      hdrs 

  in
  aux ct []
      


                 
(*
module IO (F: Mirage_flow_lwt.S) = struct


  let read_res r =
    match r with
    | `Data d -> Ok d
    | `Eof -> Error "end of stream"

  
  let read flow buf =

    let rec aux b ct hdrs =
      let h = from_buf ~ct b in
      
      match h with
      | `Out_of_bounds (i, h2) ->
         F.read flow >>= fun res ->

         if (result_ok res) then
           let cs = fmap_result res read_res in 
           let () = append b (cs) in
           aux b i h2
         else
           Lwt.return (Error "Error in connection" )
         

      | `Done hdrs ->
         Ok hdrs |> Lwt.return
    in
        
         

    F.read flow >>= fun r ->
    match r with
    | Ok (`Data cs) ->
       let ct = Buf.read_int16 buf in
       aux buf ct []

    | _ ->
       Error "unable to parse" |> Lwt.return



  let write flow hdrs =
    let buf = create 1024 in
    encode hdrs buf;
    F.write flow buf.buf >>= fun res ->
    
    match res with
    | Ok _ -> Lwt.return_unit

    | Error _ ->
       F.close flow >>= fun _ ->
       Lwt.fail_with "Unable to send"
                     
       
end


    

  
 *)

                 

      (*
      let c1 = c - 1 in

      match (read_kv buf) with
      | Ok kv ->
         let h1 = hdrs @ [kv] in
         aux c1 h1
             
      | Error _ ->
         `Out_of_bounds c headers
    else
      
      hdrs

module IO (C: Mirage_channel_lwt.S) = struct


  let size = 1024

  let read_i16 chan =
    C.read_some chan 4 |> (fun b -> Cstruct.BE.read_uint16 b 0)
                                              
  let read_kv chan =
    
    read_i16 |> (fun i -> C.read_some chan i )
               
  let read chan =
    let b = C.read_some chan 4 |> (fun b -> Cstruct.BE.read_uint16 b 0) in
    
    
    
end
    
*)
