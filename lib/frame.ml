open Util


type frame = {
    mtype: int;
    tag: int32;
    body: Cstruct.t 
  }




             
let to_cstruct t =
  let bsize = Cstruct.len t.body in
  let frame_size = (4 + 1 + 4) + (bsize) in
  

  let buf_size = 4 + frame_size in
  let buf = Cstruct.create buf_size in

  
  let _ =
    write (Int32.of_int frame_size) 4 Cstruct.BE.set_uint32 buf
    |>  write t.mtype 1 Cstruct.set_uint8
          
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
    let (mtype, rest0) =  read 1 Cstruct.get_uint8 rest in
    let (tag, rest1) =  read 4 Cstruct.BE.get_uint32 rest0 in
    
    let (bsize, rest2) = read 4 Cstruct.BE.get_uint32 rest1 in

    let body = Int32.to_int bsize |> Cstruct.sub rest2 0 in

    Ok {tag; mtype; body}
        
    
    
    
    
    

  
  



     

type msg_type = [
  | `TPING
  | `RPING

  | `TINIT
  | `RINIT

  | `TREQ
  | `RREQ

  | `TCLOSE
  | `RCLOSE

  ]


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
  | _ -> `Unknown
