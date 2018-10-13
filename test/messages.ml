open Mux

open Message
    

module Gen = struct 

  let tag () = Random.int32 Int32.max_int



  let treq () =
    let tag = Random.int32 (Int32.max_int) in
    let path = Generators.list (Random.int 6) (Generators.string 11) () in
    let headers = Header_suite.gen () in

    let body = Generators.cstruct 300 () in
    let req = Message.TMSG.create ~tag ~path ~body ~headers () in
    `TMSG req 



  let rreq () =
    let tag = Random.int32 (Int32.max_int) in
    let status = Random.int 3 |> Status.of_int in 
    let headers = Header_suite.gen () in

    let body = Generators.cstruct 300 () in 
    let r = RMSG.create ~tag ~status ~headers ~body () in
    `RMSG r 


  let tping () =
    `TPING


  let gen_init () =
    let tag = Random.int32 Int32.max_int in
    let headers = Header_suite.gen () in

    INIT.create ~tag ~headers ()




  let rping () = `RPING

  let rinit () = `RINIT (gen_init ())

  let tinit () = `TINIT (gen_init () ) 

  let tclose () = `TCLOSE (tag ())

  let rclose () = `RCLOSE (tag () )

  let rerror () =
    let tag = tag () in
    let body = Generators.cstruct 300 () in 
    `RERROR ( RERROR.create tag body () )


  let tshutdown () = `TSHUTDOWN

  let rshutdown () = `RSHUTDOWN


  let gen () = Generators.variant [rping; tinit; rinit; treq; rreq; tclose; rclose; rerror; tshutdown; rshutdown;] ()

end






let pp = Fmt.of_to_string (Message.to_string)

let eq l r =
  Message.eq l r
    

let msg = Alcotest.testable pp eq 

let test_codec () =

  let test () =

    let exp = Gen.gen () in
    
    let buf = Message.to_frame exp |> Frame.to_cstruct in

    let got = Frame.of_cstruct buf |> Util.get_result |> Message.of_frame in 
    Alcotest.check msg "Testing Codec" exp got
      
  in

  Generators.repeat 1000 test


let suite = [
  "Test Codec", `Quick,  test_codec;
]



