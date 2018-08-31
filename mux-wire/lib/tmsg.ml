



type t = {
    path: string list;
    headers: (string * string) list;
    body: Cstruct.t 
  } 
           


let path_default = []
                     
let headers_default = []
                        
let body_default = Cstruct.empty


let make
      ?(path=path_default)
      ?(headers=headers_default)
      ?(body=body_default)
      () =

  {path; headers; body}



    


let add_headers t h =
  let headers = t.headers @ h in
  {t with headers}

let set_headers t h =
  {t with headers = h;}

(*
      
  let set_body t b =
    {t with body = b}


      
  let with_body t f =
    let body1 = f t.body in
    {t with body = body1}
 *)

    
let body t = t.body
let headers t = t.headers

let path t = t.path

let encode t buf =
  
  Codec.Path.encode t.path buf;
  Codec.Headers.encode t.headers buf;

  let i = Cstruct.len t.body |> Int32.of_int in 
  Buf.write_int32 buf i;
  Buf.append buf t.body
  
             
  
               
let decode buf =
  let path = Codec.Path.decode buf in
  let headers = Codec.Headers.decode buf in
  let size = Buf.read_int32 buf |> Int32.to_int in 

  let body = Buf.read_cstruct buf size in
  {path; headers; body}
  
  
  
  
  
               
               
open Lwt.Infix

       
module IO (F: Mirage_flow_lwt.S) = struct
  type flow = F.flow


  type t = {
      path: string list;
      headers: (string * string) list;
      body: Cstruct.t 
    } 

  let handle_read flow =
    F.read flow >>= fun r ->
    match r with
    | Ok (`Data x) -> Lwt.return x

    | Ok `Eof ->
       F.close flow >>= fun () ->
       Lwt.fail_with "connection closed by peer"

    | Error e ->
       let emsg = Fmt.strf "%a" F.pp_error e in
       F.close flow >>= fun () ->
       Lwt.fail_with emsg 
                  



  let rec read_frame flow buf =
    let hlen = Buf.read_int32 buf|> Int32.to_int in

    let rec aux () =
      let bs = Buf.len buf in
      
      if hlen <= bs then Lwt.return ()
                                    
      else
        handle_read flow >|= fun cs ->
        Buf.append buf cs

    in
    aux ()

        
        
    



  (*Just wait you get the entire message *)
                     
  let read flow =
    handle_read flow >>= fun cs ->
    let buf = Buf.of_cstruct cs in
    read_frame flow buf >|= fun () ->
    decode buf 
    
    

  
      
  let write t flow =
    let buf = Buf.create 1024 in
    encode t buf;

    let len = Buf.len buf |> Int32.of_int in 
    let cs = Cstruct.create 4 in

    Cstruct.BE.set_uint32 cs 0 len; 
    
    F.write flow cs >>= fun _ ->
    F.write flow (Buf.to_cstruct buf)
    
    
   

    

    
                
    
             
end
