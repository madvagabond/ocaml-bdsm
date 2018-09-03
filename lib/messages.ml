open Frame
open Util
       
       
module PING = struct
  type t = {tag: int32}

  let to_frame t =
    let mtype = mtype_to_int `TPING in 
    {tag=t.tag; mtype; body=Cstruct.empty}


  let tag t = t.tag
                

  
      
end

module Status = struct
  type t = [

    | `Ok
    | `Nack
    | `Error
    ]

  let to_int =
    function
    | `Ok -> 0
    | `Nack -> 1
    | `Error -> 2

  let of_int =
    function
    | 0 -> `Ok 
    | 1 -> `Nack
    | 2 -> `Error
    | _ -> `Unknown

            
          
end


module TREQ = struct
  type t = {tag: int32; path: string list; headers: Headers.t; body: Cstruct.t}

  let tag t = t.tag
  let path t = t.path

  let headers t =
    t.headers

  let body t = t.body

  let add_headers t h =
    let headers = t.headers @ h in
    {t with headers}

  let set_headers t h =
    {t with headers = h;}



  let prepare t =
    let path_string = String.concat "/" t.path in
    let hsize = Headers.buffer_size t.headers in
    let body_size = Cstruct.len t.body in

    let size = 4 + (String.length path_string) + hsize + body_size in 
    (path_string, size)
      
    

  let buf_size t =
    let (_, size) = prepare t in
    size 

      
  let to_frame t =
    let (path, size) = prepare t in
    let slen = String.length path in
    
    let body = Cstruct.create size in

    let (blen, bsize) =
      let bs = Cstruct.len t.body in
      let bl = Int32.of_int bs in
      (bl, bs)
    in
    

    let _ = 
      Headers.write t.headers body
      |> write slen 2 Cstruct.BE.set_uint16 
      |> write_string path
      |> write blen 4 Cstruct.BE.set_uint32
               
      |> (fun c ->
        
        Cstruct.blit t.body 0 c 0 bsize;
        Cstruct.shift c bsize
      )

    in
    
    let tag = t.tag in
    let mtype = mtype_to_int `TREQ in
    {tag; mtype; body}      

    
    
                 

                 
end

module RMSG = struct
  type t = {tag: int32; status: int; headers: Headers.t; body: Cstruct.t}
              
end 

module INIT = struct
  type t = {tag: int; headers: Headers.t;}
end 

type t = [

  |  `TPING of int32

  | `RPING of int32 

  | `TINIT of INIT.t
  | `RINIT of INIT.t

  | `TREQ of TREQ.t
  | `RREQ of RMSG.t

  | `TCLOSE of int32
  | `RCLOSE of int32
                 

  ]
