open Mux.Headers




let gen () =
  let open Generators in 
  list 10 ( pair (string 100) (string 100) ) () 
  



let pp_headers =
  let kv = Fmt.pair Fmt.string Fmt.string in
  Fmt.list kv 


let eq l r = l = r

let headers_t =
  Alcotest.of_pp pp_headers 

let test_codec () =
  let headers = [("test", "123")] in
  let got = to_cstruct headers |> of_cstruct in
  Alcotest.check headers_t "same headers" headers got

let test_reads () =
  let open Alcotest in
  
  let h = [
      "hello", "friend";
      "hello", "world";
      "satan", "loves you"
    ]
  in

  let _ =
    let got = get h "hello" in
    let expected = Some "friend" in
    Alcotest.check (option string) "Gets work" got expected
  in


  let _ =
    let got = get_multi h "hello" in
    let expected = ["friend"; "world"] in
    Alcotest.check (list string) "get_multi return all values of key" got expected
  in


 let _ = 
    let got = get_list h ["hello"; "satan"] in
    Alcotest.check headers_t "get_list returns correct subset of headers" got h
 in

 ()
                 
  

let test_writes () =
  let open Alcotest in 
  
  let _ =
    let h = [] in
    
    let values = ["friend";"world"] in 
    let got = add_multi h "hello" values in

    let exp =
      [
        "hello", "friend";
        "hello", "world";
      ] in

    Alcotest.check
      headers_t
      "all entries for key were added"
      got
      exp

  in

  let _ =
  
    let h = [
        "hello", "friend";
        "hello", "world";
        "satan", "loves you"
      ] in

    let h0 = remove h "satan" in
    let got = get h0 "satan" in

    Alcotest.check (option string) "key was removed" got None
  in
  

  ()


   
let tests = [
    "Test Codec", `Quick, test_codec;
    "Test Reads", `Quick, test_reads;
    "Test Writes", `Quick, test_writes;                 
  ]
