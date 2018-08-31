open Buf
       

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
