type t = {mutable buffer: Cstruct.t }

  

let shift t off =
  let buf = Cstruct.of_bigarray
    ~off:(t.buffer.Cstruct.off + off)
    ~len:(t.buffer.Cstruct.len - off)
    t.buffer.Cstruct.buffer  in
  t.buffer <- buf


let reset t  =
  let rem = 0 - t.buffer.Cstruct.off in
  shift t rem
  

let write t size f c =


  let rem = (Cstruct.len t.buffer) - size in

  begin 
    if rem < 0 then
      let buf_1 = Cstruct.create (Cstruct.len t.buffer) in
      t.buffer <- Cstruct.append t.buffer buf_1 
    else
      ()
        
  end;
    
  f t.buffer 0 c;
  shift t size
                
      


let write_int16 buf i =
  write buf 2 Cstruct.BE.set_uint16 i

                                 

let write_string t str =
  let len = String.length str in
  
  write t len (fun _ _ _ ->
      Cstruct.blit_from_string str 0 t.buffer 0 len;
) str

      
                                 
let read t len f =
  let out = f t.buffer 0 in
  shift t len; 
  out 


let read_string t len =
  if len = 0 then ""
  else
    let str = Bytes.create len in
    read t len (fun _ _ ->
        Cstruct.blit_to_string t.buffer 0 str 0 len;
      );
    Bytes.to_string str


let read_int16 t =
  read t 2 Cstruct.BE.get_uint16


let of_cstruct buffer =
  {buffer}

let append t buf =
  t.buffer <- Cstruct.append t.buffer buf


let create size =
  Cstruct.create size |> of_cstruct


let len t =
  Cstruct.len t.buffer

let off t=
  t.buffer.Cstruct.off


let write_int32 t i =
  write t 4 Cstruct.BE.set_uint32 i 

let read_int32 t =
  read t 4 Cstruct.BE.get_uint32 


let read_cstruct t len  =
  let ret = Cstruct.sub t.buffer 0 len in
  t.buffer <- Cstruct.shift t.buffer len;
  ret 


let prepend t cs =
  t.buffer <- Cstruct.append cs t.buffer 

                             
let to_cstruct t =
  let neg = 0 - (len t) in
  shift t neg;
  t.buffer
    
