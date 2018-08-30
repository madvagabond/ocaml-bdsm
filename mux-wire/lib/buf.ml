type t = {mutable buf: Cstruct.t }

  

let shift t off =
  let buf = Cstruct.of_bigarray
    ~off:(t.buf.Cstruct.off + off)
    ~len:(t.buf.Cstruct.len - off)
    t.buf.Cstruct.buffer  in
  t.buf <- buf


let reset t  =
  let rem = 0 - t.buf.Cstruct.off in
  shift t rem
  

let write t size f c =


  let rem = (Cstruct.len t.buf) - size in

  begin 
    if rem < 0 then
      let buf_1 = Cstruct.create (Cstruct.len t.buf) in
      t.buf <- Cstruct.append t.buf buf_1 
    else
      ()
        
  end;
    
  f t.buf 0 c;
  shift t size
                
      


let write_int16 buf i =
  write buf 4 Cstruct.BE.set_uint16 i

                                 

let write_string t str =
  let len = String.length str in
  
  write t len (fun _ _ _ ->
      Cstruct.blit_from_string str 0 t.buf 0 len;
) str

      
                                 
let read t len f =
  let out = f t.buf 0 in
  shift t len; 
  out 


let read_string t len =
  if len = 0 then ""
  else
    let str = Bytes.create len in
    read t len (fun _ _ ->
        Cstruct.blit_to_string t.buf 0 str 0 len;
      );
    Bytes.to_string str


let read_int16 t =
  read t 4 Cstruct.BE.get_uint16
