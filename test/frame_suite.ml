open Mux

let fmt =
  let open Frame in 
  let to_string r =
    Fmt.strf "%d %ld %s" (Frame.mtype_to_int r.mtype) (Frame.tag r) (Cstruct.to_string r.body)in

  Fmt.of_to_string to_string


let eq l r =
  let open Frame in
  
  let mte = (Frame.mtype_to_int l.mtype) = (Frame.mtype_to_int r.mtype) in
  let te = l.tag = r.tag in
  let body = Cstruct.equal l.body r.body in

  mte && te && body

  


let frame =
  Alcotest.testable fmt eq


let (^) i e =
  let rec aux n e =
    if (e > 0) then
      aux (n * i) (e - 1)
    else
      n

  in

  aux i e



    
  


let gen_frame () =
  let mtype = Random.int 10  |> Frame.mtype_of_int in
  let tag = Random.int32 Int32.max_int in
  let size = Random.int (10 ^ 6) in
  
  let body = Generators.cstruct size () in

  Frame.make mtype tag body


  

  
  

let test_codec x =

  let rec loop i = 
    let f = Frame.make `TMSG 0l Cstruct.empty in

    let buf = Frame.to_cstruct f in
    let got = Frame.of_cstruct buf |> Util.get_result in

    Alcotest.check frame "Decode Frame" got f;

    if i > 0 then loop (i - 1)
    else ()

  in
  loop 10






let tests = [
  "Test Codec", `Quick, test_codec;
]
