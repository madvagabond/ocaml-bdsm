open Buf
       
type t = (string * string) list

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
  (k, v)
    
                           
let encode t buf =
  let card = List.length t in
  write_int16 buf card;

  List.iter (fun kv ->
      write_kv buf kv 
    ) t
  
  
  

let decode buf =
  let cnt = Buf.read_int16 buf in

  let rec aux c hdrs =

    if c >= 1 then 
      let c1 = c - 1 in
      let (k, v) = read_kv buf in 
    

      let hdrs1 = hdrs @ [(k, v)] in
      aux c1 hdrs1

    else
      hdrs

  in
  aux cnt []
    

                           

      

  
           
