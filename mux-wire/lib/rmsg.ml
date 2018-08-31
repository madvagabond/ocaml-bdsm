 type rmsg = {
      headers: (string * string) list;
      body: Cstruct.t
   }

 type t = rmsg
               

              
                      
 let headers_default = []
                         
 let body_default = Cstruct.empty


 let make
       ?(headers=headers_default)
       ?(body=body_default)
       () =
   {headers; body}



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


 let encode t buf =
     Codec.Headers.encode t.headers buf;

  let i = Cstruct.len t.body |> Int32.of_int in 
  Buf.write_int32 buf i;
  Buf.append buf t.body
  
             


 let decode buf =
  let headers = Codec.Headers.decode buf in
  let size = Buf.read_int32 buf |> Int32.to_int in 

  let body = Buf.read_cstruct buf size in
  {headers; body}





 open Lwt.Infix

        
 module IO (F: Mirage_flow_lwt.S) = struct
   type flow = F.flow

   module Frame = Codec.Frame(F)


   type t = rmsg

              

   let read flow =
     Frame.read flow decode

   let write flow t  =
     let buf = Buf.create 1024 in
     encode t buf;

     Frame.write flow buf
                 


                 
                 
                 
 end
