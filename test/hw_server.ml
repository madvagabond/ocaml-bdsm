open Mux
open Messages
open Lwt.Infix

open OUnit2
       

       
module Stack = Tcpip_stack_socket


                 
type stack = {
  stack : Stack.t;
  tcp   : Tcpv4_socket.t;
  udp : Udpv4_socket.t;
}
                 
       
let make_stack ~name ~ip =
  (* define a config record, which should match the type expected of
     Mirage_stack_lwt.stackv4_config *)
  Tcpv4_socket.connect (Some ip) >>= fun tcp ->
  Udpv4_socket.connect (Some ip) >>= fun udp ->
  let config = {
      Mirage_stack_lwt.name;
      interface = [ip];
    } in
  
  Stack.connect config udp tcp >>= fun stack ->
  Lwt.return { stack; udp; tcp }

             
       
module TCPV4Listener = struct
  include Tcpip_stack_socket.TCPV4


  type callback = flow -> unit Lwt.t

  let ip =  Ipaddr.V4.of_string_exn "0.0.0.0"

  let stack = make_stack ~name: "server" ~ip
                         
  let listen ~port cb =
    stack >>= fun server -> 
    Stack.listen_tcpv4 server.stack ~port cb;
    Stack.listen server.stack

end 

module Server = Server.Make(TCPV4Listener)




module Client = Mux.Client.Make(Stack.TCPV4)

                               

                           

let cb req =
  print_endline "working";
  let name = TMSG.body req |> Cstruct.to_string in
  let body = Fmt.strf "Hello %s" name |> Cstruct.of_string in
  RMSG.make ~body () |> Lwt.return


                       

let get_ok res =
   res |> Base.Result.ok |> Batteries.Option.get


let localhost = Ipaddr.V4.of_string_exn "0.0.0.0"
                                        


                       
let connect () =
  Tcpv4_socket.connect (Some localhost) >>= fun tcp ->
  Tcpv4_socket.create_connection tcp (localhost, 5000) >|= get_ok
                                                                   
                                                                   

let test ctx =

  let path = ["hello"; "world"] in
  let body = Cstruct.of_string "friend" in
  
  let req = TMSG.make ~path ~body () in
  
  connect () >>= fun flow ->
  Client.send_recv flow req >>= fun rep ->

  let expected = Cstruct.of_string "Hello friend" in
  let got = rep |> RMSG.body in

  assert_bool "bodies are unequal" (Cstruct.equal got expected) |> Lwt.return



let listener () =
  let handler = Server.make_handler ~cb () in
  Server.listen ~port:5000 ~handler () 


