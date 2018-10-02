open Frame
open Util



       
let ping_tag = 0l





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
    | _ -> failwith "no such status"

            

  let to_string =
    function
    | `Ok -> "ok"
    | `Nack -> "nack"
    | `Error -> "error"
      
end



let random_tag () = Random.int32 Int32.max_int







module TREQ = struct

  type t = {
    tag: int32;
    path: string list;
    headers: Headers.t;
    body: Cstruct.t
  }

  

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



  let buffer_size t =
    let path_string = String.concat "/" t.path in
    let hsize = Headers.buffer_size t.headers in
    let body_size = Cstruct.len t.body in

    let size = 2 + (String.length path_string) + hsize + body_size in 
    size
      


  let create ?(path=[]) ?(headers=[]) ?(tag = random_tag ())  ?(body= Cstruct.empty) () =
    {path; headers; tag; body}



  let to_body t =
    let size = buffer_size t in
    
    let path = String.concat "/" t.path in
    let slen = String.length path in
    
    let buf = Cstruct.create size in
    let bsize = Cstruct.len t.body in


    
    let _ =
      (*      write len 4 Cstruct.BE.set_uint32 buf *)
      Headers.write t.headers buf
      |> write slen 2 Cstruct.BE.set_uint16 
      |> write_string path
               
      |> (fun c ->
        
        Cstruct.blit t.body 0 c 0 bsize;
        Cstruct.shift c bsize
      )

    in buf

  
  let of_frame f =

    let tag = Frame.tag f in
    let body = Frame.body f in 
    
    let (headers, b1) = Headers.read body in
    let (path_l, b2) = read 2 Cstruct.BE.get_uint16 b1 in
    let (path_s, b3) =  read_string path_l b2 in 

  
    let body =
      let len = Cstruct.len b3 in 
      Cstruct.sub b3 0 len
    in

    let path = Str.split (Str.regexp "/") path_s in
    
    
    {tag; path; headers; body}

         
  let to_string t =
    let h = Headers.to_string t.headers in
    let body = Cstruct.to_string t.body in 
    Fmt.strf "treq \n %s \n %ld \n\n %s \n\n %s"  (String.concat "/" t.path) t.tag h body 


                 

                 
end















module RREQ = struct
  type t = {tag: int32; status: Status.t; headers: Headers.t; body: Cstruct.t}

  let buffer_size t =
    1 + (Headers.buffer_size t.headers) +  (Cstruct.len t.body)



  let tag t = t.tag

  let status t = t.status

  let headers t = t.headers

  let body t = t.body

  let create
      ?(tag= random_tag () )
      ?(status=`Ok)
      ?(headers = Headers.init ())
      ?(body = Cstruct.empty) () =
    {tag; headers; status; body}

                 
                    

  let to_body t =
    let size =  buffer_size t in
    
    let buf = Cstruct.create size in
    let body_s = Cstruct.len t.body in


    let _ = 
      (*write len 4 Cstruct.BE.set_uint32 buf |> *)
      Headers.write t.headers buf 
      |> write (Status.to_int t.status) 1 Cstruct.set_uint8 
      |> (fun c ->
        
        Cstruct.blit t.body 0 c 0 body_s;
        Cstruct.shift c body_s
      )
    in
    buf


  
  let of_frame f =
    let tag = Frame.tag f in
    let (headers, b1) = Headers.read f.body in
    let (stat_i, b2) = read 1 Cstruct.get_uint8 b1 in

    Printf.printf "%d\n" stat_i;
    let body =
      let len = Cstruct.len b2 in 
      Cstruct.sub b2 0 len
    in

    let status = Status.of_int stat_i in 
    {tag; status; headers; body}




  let to_string t =
    let s = Status.to_string t.status in
    let body = Cstruct.to_string t.body in
    let hdrs = Headers.to_string t.headers in

    Fmt.strf "rreq\n\n%ld\n\n%s\n\n%s\n\n%s\n" t.tag s hdrs body

  

 
         
end 













module INIT = struct
  type t = {tag: int32; headers: Headers.t;}

  let to_body t =
    Headers.to_cstruct t.headers

  let of_frame f =
    let tag = Frame.tag f in
    let headers = Frame.body f |> Headers.of_cstruct in
    {tag; headers}

  let tag t = t.tag
                
  let headers t = t.headers
                    
  let set_headers t headers =
    {t with headers}

  let add_headers t h =
    let headers = t.headers @ h in
    {t with headers}
    

  let set_headers t h =
    {t with headers = h;}


  let create ?(tag = random_tag () ) ?(headers=[]) () =
    {tag; headers}



  let to_string t =
    Fmt.strf "%ld \n %s" (t.tag) (Headers.to_string t.headers) 
                       
end












module RERROR = struct
  type t = {tag: int32; body: Cstruct.t}

  let tag t = t.tag
  let body t = t.body

  let to_frame t =
    let mtype = `RERROR in 
    {mtype; tag= t.tag; body = t.body}

  let of_frame f =
    let tag = Frame.tag f in
    let body = Frame.body f in

    {tag; body}


  let create ~tag ~body () =
    {tag;body}
end



















type t = [

  | `TPING

  | `RPING 

  | `TINIT of INIT.t
  | `RINIT of INIT.t

  | `TREQ of TREQ.t
  | `RREQ of RREQ.t

  | `TCLOSE of int32
  | `RCLOSE of int32

  | `TSHUTDOWN
  | `RSHUTDOWN

  | `RERROR of RERROR.t
                 

  ]







let tag =
  function 
  | `TPING -> 0l
  | `RPING -> 0l

  | `TINIT x -> INIT.tag x
  | `RINIT x -> INIT.tag x

  | `TREQ x -> TREQ.tag x                      
  | `RREQ x -> RREQ.tag x

  | `TCLOSE x -> x
  | `RCLOSE x -> x
    
  | `TSHUTDOWN -> 0l
  | `RSHUTDOWN -> 0l

  | `RERROR t -> RERROR.tag t
    












     
let to_frame =
  function

  | `TPING ->
     let mtype = `TPING in 
     {mtype; tag=0l; body=Cstruct.empty}


  | `RPING ->
     let mtype = `RPING in
     {mtype; tag=0l; body=Cstruct.empty}

  | `TINIT init ->
     let body = INIT.to_body init in
     let mtype = `TINIT in
     let tag = INIT.tag init in 
     {mtype; tag; body;}

  | `RINIT init ->
     let body = INIT.to_body init in
     let mtype = `RINIT in
     let tag = INIT.tag init in 
     {mtype; tag; body;}
       

  | `TREQ t ->
     let body = TREQ.to_body t in
     let mtype = `TREQ in
     let tag = TREQ.tag t in
     {body; tag; mtype}

  | `RREQ t ->
     let body = RREQ.to_body t in
     let mtype = `RREQ in
     let tag = RREQ.tag t in
     {body; tag; mtype}


  | `TCLOSE i ->
     let mtype = `TCLOSE in
     let tag = i in
     let body = Cstruct.empty in

     {mtype; tag; body}

  | `RCLOSE i ->
     let mtype = `RCLOSE in
     let tag = i in
     let body = Cstruct.empty in

     {mtype; tag; body}


  | `TSHUTDOWN ->
    let mtype = `TSHUTDOWN in
    {mtype; tag=0l; body=Cstruct.empty}

  | `RSHUTDOWN ->
    let mtype = `RSHUTDOWN in 
    {mtype; tag=0l; body=Cstruct.empty}

  | `RERROR t ->
    RERROR.to_frame t 
    
  
 

       




let of_frame f =
  let tag = Frame.tag f in
  let mtype = Frame.mtype f in 
  match mtype with
  | `TPING -> `TPING
  | `RPING -> `RPING

  | `TINIT -> `TINIT (INIT.of_frame f)
  | `RINIT -> `RINIT (INIT.of_frame f)

  | `TREQ -> `TREQ (TREQ.of_frame f)
  | `RREQ -> `RREQ (RREQ.of_frame f)

  | `TCLOSE -> `TCLOSE tag
  | `RCLOSE -> `RCLOSE tag
                 
  | `RERROR ->
    `RERROR (RERROR.of_frame f)
      
  | `TSHUTDOWN -> `TSHUTDOWN

  | `RSHUTDOWN -> `RSHUTDOWN
    
    

type rping = [`RPING]
type tping = [`TPING]    
               
type ping = [`TPING | `RPING]
type req = [ `TREQ of TREQ.t | `RREQ of RREQ.t]

type tmsg = [`TREQ of TREQ.t | `TPING of int32]
type rmsg = [`RREQ of RREQ.t | `RPING of int32]

type treq = [`TREQ of TREQ.t]
type rreq = [`RREQ of RREQ.t]

type rerror = [`RERROR of RERROR.t]

type init = [`TINIT of INIT.t | `RINIT of INIT.t]

type shutdown = [`TSHUTDOWN | `RSHUTDOWN]
                


type close =
  [
    `TCLOSE of int32 |
    `RCLOSE of int32
  ]



let to_string = function
  | `TPING -> "tping"
  | `RPING -> "rping"
  | `TCLOSE i -> Fmt.strf "tclose %ld" i
  | `RCLOSE i -> Fmt.strf "tclose %ld" i

  | `TINIT init -> Fmt.strf "tinit \n %s \n" (INIT.to_string init)

  | `RINIT init -> Fmt.strf "rinit \n %s" (INIT.to_string init)
  | `TREQ t -> TREQ.to_string t

  | `RREQ t -> RREQ.to_string t


  | `RERROR t -> Fmt.strf "rerror %ld \n %s" (RERROR.tag t) (RERROR.body t |> Cstruct.to_string)
  | `RSHUTDOWN -> "rshutdown"
  | `TSHUTDOWN -> "tshutdown"












let eq l r =
  match (l, r) with
  | (`TPING, `TPING) -> true
  | (`RPING, `RPING) -> true
  | (`TINIT il, `TINIT ir) -> il = ir
  | (`RINIT il, `RINIT ir) -> il = ir

  | (`TREQ tl, `TREQ tr) ->
    let open TREQ in
    let be = Cstruct.equal tl.body tr.body in
    let he = tl.headers = tr.headers in
    let path = tl.path = tr.path in
    be && he && path 

  | (`RREQ tl, `RREQ tr) ->
    let open RREQ in
   
    let be = Cstruct.equal tl.body tr.body in
    let he = tl.headers = tr.headers in

    let se = (Status.to_int tl.status) = (Status.to_int tl.status) in

    
    be && he && se

  | (`TCLOSE il, `TCLOSE ir) -> il = ir

  | (`RCLOSE il, `RCLOSE ir) -> il = ir

  | (`TSHUTDOWN, `TSHUTDOWN) -> true

  | (`RSHUTDOWN, `RSHUTDOWN) -> true

  | (`RERROR tl, `RERROR tr) ->
    let open RERROR in
    let te = tl.tag = tr.tag in
    let be = Cstruct.equal tl.body tr.body in
    te && be

  | _ -> false




