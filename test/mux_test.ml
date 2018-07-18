open OUnit2
open Mux
open Messages
       
open Codec_util
       
       

module Option = Batteries.Option


let rmsg_equal l r =
  let open RMSG in
  let pp x = Headers.to_string x.headers in
  
  (pp r = pp l) && (Cstruct.equal l.body r.body)


let tmsg_equal l r =
  let open TMSG in
  let pp_path x = Path.to_string x.path in 
  let pp x = Headers.to_string x.headers in
  (pp r = pp l) && (Cstruct.equal l.body r.body) && (pp_path l = pp_path r)
                     

                     
let rmsg_codec ctx =
  let headers =
    [
      ("bitch", "ass");
      ("punk", "motherfucker")
    ] in

  let body = "test_body" |> Cstruct.of_string in
  let rmsg = RMSG.make ~headers ~body () in

  let bytes = RMSG.encode rmsg  in


  let got =
    RMSG.decode bytes |> Base.Result.ok  |> Option.get
  in

  assert_bool "decoded message has same contents" (rmsg_equal got rmsg)
  


let header_codec ctx =
  let hdrs = [
      ("faggot", "bitch");
      ("cuck", "bitch")
    ]
  in


  let got = hdrs |> Headers.to_string |> Headers.from_string in
  assert_equal hdrs got


let tmsg_codec ctx =

  let headers =
    [
      ("bitch", "ass");
      ("punk", "motherfucker")
    ] in

  let path = ["hello"; "world"] in
  let body = "test_body" |> Cstruct.of_string in
  let tmsg = TMSG.make ~path ~headers ~body () in

  let bytes = TMSG.encode tmsg  in


  let got =
    TMSG.decode bytes |> Base.Result.ok |> Option.get
  in

  assert_bool "decoded message has same contents" (tmsg_equal got tmsg)
  
                                         
  
let suite =
  "Codec Suite" >:::
    [
      "rmsg codec test" >:: rmsg_codec;
      "header codec test" >:: header_codec;
      "tmsg codec" >:: tmsg_codec
    ]

let () =
  run_test_tt_main suite




