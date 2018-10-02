open Util


type msg_type = [
  | `TPING
  | `RPING

  | `TINIT
  | `RINIT

  | `TREQ
  | `RREQ

  | `TCLOSE
  | `RCLOSE
    
  | `TSHUTDOWN
  | `RSHUTDOWN

  | `RERROR

  ]



       
type t = {
    mtype: msg_type;
    tag: int32;
    body: Cstruct.t 
  }

let tag f = f.tag
let body f = f.body
let mtype f = f.mtype
                
let make mtype tag body = {mtype; tag; body}

let mtype_to_int =
  function

  | `TPING -> 0
  | `RPING -> 1

  | `TINIT -> 2
  | `RINIT -> 3

  | `TREQ -> 4
  | `RREQ -> 5

  | `TCLOSE -> 6
  | `RCLOSE -> 7

  | `TSHUTDOWN -> 8
  | `RSHUTDOWN -> 9 


  | `RERROR -> 10
                 
let mtype_of_int =
  function
  | 0 -> `TPING
  | 1 -> `RPING

  | 2 -> `TINIT
  | 3 -> `RINIT

  | 4 -> `TREQ
  | 5 -> `RREQ

  | 6 -> `TCLOSE
  | 7 -> `RCLOSE
    
  | 8 -> `TSHUTDOWN
  | 9 -> `RSHUTDOWN

  | 10 -> `RERROR
    
  | i ->
     Invalid_argument (Printf.sprintf "message type %d unknown" i) |> raise



             
let to_cstruct t =
  let bsize = Cstruct.len t.body in
  let frame_size = (4 + 1 + 4) + (bsize) in
  

  let buf_size = 4 + frame_size in
  let buf = Cstruct.create buf_size in

  let mt = mtype_to_int t.mtype in 
  
  let _ =
    write (Int32.of_int frame_size) 4 Cstruct.BE.set_uint32 buf
    |>  write mt 1 Cstruct.set_uint8
          
    |> write t.tag 4 Cstruct.BE.set_uint32
    |> write (Int32.of_int bsize) 4 Cstruct.BE.set_uint32
             
    |> (fun c ->
      Cstruct.blit t.body 0 c 0 bsize;
      Cstruct.shift c bsize
                    
      )                             
  in

  buf




    
    
  
let of_cstruct buf =
  let (size, rest) = read 4 Cstruct.BE.get_uint32 buf in
  let s = Int32.to_int size in

  if (Cstruct.len rest) < s then
    Error "Not enough space to decode"
          
  else
    let (mt, rest0) =  read 1 Cstruct.get_uint8 rest in
    let (tag, rest1) =  read 4 Cstruct.BE.get_uint32 rest0 in
    
    let (bsize, rest2) = read 4 Cstruct.BE.get_uint32 rest1 in

    let body = Int32.to_int bsize |> Cstruct.sub rest2 0 in

    let mtype = mtype_of_int mt in
    Ok {tag; mtype; body}
        
    
    
    
    
    

  
  



     

