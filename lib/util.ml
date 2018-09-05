let write c len f t =
  f t 0 c;
  Cstruct.shift t len 
  

let read len f t =
  let got = f t 0  in
  let rest = Cstruct.shift t len in
  (got, rest)
  


let write_string str t=
  
  let len = String.length str in
  Cstruct.blit_from_string str 0 t 0 len;
  Cstruct.shift t len 
  

let read_string len t =
  let got = Cstruct.copy t 0 len in
  got, (Cstruct.shift t len)
