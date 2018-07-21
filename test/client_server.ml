open Lwt.Infix
open OUnit2
       

                       
module Stack = Tcpip_stack_socket
module Client = Mux.Client.Make(Stack.TCPV4)
                               
open Mux.Messages

                 
type stack = {
    stack : Stack.t;
    icmp  : Icmpv4_socket.t;
    udp   : Udpv4_socket.t;
    tcp   : Tcpv4_socket.t;
  }


let addr =  Ipaddr.V4.of_string_exn "0.0.0.0"
let localhost = Ipaddr.V4.of_string_exn "127.0.0.1"
               
let make_stack ~name ~ip =
  (* define a config record, which should match the type expected of
     Mirage_stack_lwt.stackv4_config *)
  Tcpv4_socket.connect (Some ip) >>= fun tcp ->
  Udpv4_socket.connect (Some ip) >>= fun udp ->
  let config = {
      Mirage_stack_lwt.name;
      interface = [ip];
    } in
  Icmpv4_socket.connect () >>= fun icmp ->
  Stack.connect config udp tcp >>= fun stack ->
  Lwt.return { stack; icmp; udp; tcp }





let stack = make_stack ~name:"client" ~ip:addr

                       
let get_ok res =
   res |> Base.Result.ok |> Batteries.Option.get


             
let connect () =
  stack >>= fun client ->
  print_endline "connecting";
  Stack.TCPV4.create_connection client.tcp (localhost, 5000) >|= get_ok
 
                                                                            

let test ctx =

  let path = ["hello"; "world"] in
  let body = Cstruct.of_string "friend" in
  
  let req = TMSG.make ~path ~body () in
  connect () >>= fun flow ->
  Client.send_recv flow req >>= fun rep ->

  let msg = rep |> RMSG.body |> Cstruct.to_string in
  assert_equal "hello friend" msg |> Lwt.return
