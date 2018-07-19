open OUnit2
open Mux
open Messages
       
open Codec

open Lwt.Infix
       

module Option = Batteries.Option

module Flow = Mirage_flow_unix.Fd
module RDR = Codec.Reader(Flow)(TMSG)


let rmsg_equal l r =
  let open RMSG in
  let pp x = Headers.to_string x.headers in
  
  (pp r = pp l) && (Cstruct.equal l.body r.body)



let make_wrtr () =
  let fd =
    let open Lwt_unix in
    let fd_flags = [O_CREAT; O_WRONLY; O_APPEND; O_TRUNC] in
    Lwt_unix.openfile "/tmp/rdr_tst" fd_flags 0o777
  in

  fd 

let make_rdr () =
  let fd =
    let open Lwt_unix in
    let fd_flags = [O_RDONLY] in
    Lwt_unix.openfile "/tmp/rdr_tst" fd_flags 0o777
  in

  fd 
  


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

  assert_bool "decoded message doesn't have same contents" (rmsg_equal got rmsg)
  


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
  let bytes_1 = Cstruct.append Cstruct.empty bytes in 

  let got =
    TMSG.decode bytes_1 |> Base.Result.ok |> Option.get
  in

  assert_bool "decoded message doesn't have same contents" (tmsg_equal got tmsg)



let get_buf r =
  match r with
  | Ok (`Data x) -> Lwt.return x
  | Ok `Eof -> Lwt.fail_with "EOF before message could be read"
  | Error e -> Lwt.fail_with "Error occured before it could be read"


                             

  
  

  

let reader_test ctx =

  let headers =
    [
      ("bitch", "ass");
      ("punk", "motherfucker")
    ] in

  let path = ["hello"; "world"] in
  let body = "test_body" |> Cstruct.of_string in
 
  let req =
    TMSG.make
      ~path
      ~headers
      ~body ()
  in 


  (*fd >>= fun flow ->*)
  let buf = TMSG.encode req in

  make_wrtr () >>= fun wflow ->
  Flow.write wflow buf >>= fun _ ->
  Flow.close wflow >>= fun () -> 

  make_rdr ()  >>= fun rflow -> 
  RDR.read rflow >>= fun resp ->
  
  let got = resp |> Base.Result.ok |> Option.get in
  assert_bool "values aren't equal" (tmsg_equal got req);
  Flow.close rflow 
   


let run_async f ctx =
  Lwt_main.run (f ctx)
              
  
let suite =
  "Codec Suite" >:::
    [
      "rmsg codec test" >:: rmsg_codec;
      "header codec test" >:: header_codec;
      "tmsg codec" >:: tmsg_codec;
      "reader test" >:: (run_async reader_test)

    ]

let () =
  run_test_tt_main suite


