open Buf
open Lwt.Infix
       
       

module Headers = struct
  type t = Mux.Headers.t

             
  let write_kv buf kv =
    let (k, v) = kv in
    let k_l = String.length k in
    let v_l = String.length v in

    Buf.write_int16 buf k_l;
    write_string buf k;

    
    
    Buf.write_int16 buf v_l;
    write_string buf v
                 

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


  let decode buf =
    let ct = Buf.read_int16 buf in 
    
    let rec aux c hdrs =
      if c > 0 then
        let c1 = c - 1 in 
        let kv = read_kv buf in

        aux c1 (hdrs @ [kv])
            
            

      else
        hdrs 

    in
    aux ct []
      
    
end


module Path = struct

  type t = string list 
  
  let encode t buf =
    let path = String.concat "/" t in 
    let size = String.length path in
    Buf.write_int16 buf size;
    Buf.write_string buf path


  let decode buf =
    let size = Buf.read_int16 buf in
    Buf.read_string buf size |> Str.split (Str.regexp "/")


end 

                

module Frame (Flow: Mirage_flow_lwt.S) = struct
  
  type t = {
      path: string list;
      headers: (string * string) list;
      body: Cstruct.t 
    } 


             
            
             


  (*Just wait you get the entire message *)
                     
  let read flow decode =
    Flow.read flow >>= fun r ->
    match r with

    | Ok (`Data cs) -> 
    
       let buf = Buf.of_cstruct cs in

       Util.read_frame
         (fun () -> Flow.read flow)
         (fun () -> Flow.close flow)
         buf >|= fun res ->
       
       Util.map_result res (fun () -> decode buf)


    | Ok `Eof ->
       Flow.close flow >|= fun _ ->
       Error "peer closed connection before frame could be recieved"
                        
    | Error e ->
       Flow.close flow >|= fun _ -> 
       Error "an error has occured"



  let handle_write flow buf =
    Flow.write flow buf >>= fun r -> 
    match r with
    | Ok x -> Lwt.return x
                
    | Error _ ->
       Flow.close flow >>= fun _ ->
       Lwt.fail_with "Tried to write to closed connection"
                   
   
  let write flow buf  =
    let len = Buf.size buf |> Int32.of_int in
    let cs = Cstruct.create 4 in

    Cstruct.BE.set_uint32 cs 0 len;
    handle_write flow cs >>= fun _ ->
    handle_write flow (Buf.to_cstruct buf)
    
   
    
    
    
   

    

end
