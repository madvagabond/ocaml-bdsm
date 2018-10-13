open Lwt.Infix
open Mux
    
module Transport = struct

  type peer = string

  type conn = {
    peer: string;
    ic: Message.t Lwt_queue.t;
    oc: Message.t Lwt_queue.t
  }


 

  let create peer =
    let ic = Lwt_queue.create () in
    let oc = Lwt_queue.create () in 
    {peer; ic; oc}


  let pipe l r =
    let tx = create l in
    let rx = {peer = r; ic = tx.oc; oc = tx.ic} in
    (tx, rx)

  
  let write conn msg =
    Lwt_queue.put conn.oc msg


  let read conn =
    Lwt_queue.poll conn.ic

  let close conn =
    Lwt_queue.close conn.oc


  let dst conn = conn.peer

end


module SM = SessionManager.Make(Transport)



module type NET = sig
  
  include S.Connector
  include S.Listener with type conn := conn and type peer := peer
    
end


module Net  = struct

  

  include Transport


  type conn_t = {src: peer; dst: peer}

  type cb = conn -> unit Lwt.t


  type listeners = (peer, cb) Hashtbl.t

  let l = Hashtbl.create 1024


  let accept conn_t =
    let {src; dst} =  conn_t in

    let cb = Hashtbl.find l dst in
    let (tx, rx) = Transport.pipe src dst in
    let _ = cb rx in
    Lwt.return tx




  
  let listen peer cb =
    let _ = Hashtbl.add l peer cb in
    Lwt.return_unit


  let connect peer =
    let src = "client:" ^  (Random.int 36000 |> string_of_int) in
    let ct = {src; dst=peer} in
    accept ct

end 



module Client = Client.Make(Net)
module Server = Server.Make(Net)






let test_transport s () =

  print_endline "connected";
  
  let msg = Messages.Gen.gen () in

  let rec echo conn =
     Net.read conn >>= fun m -> 
     Net.write conn m >>= fun _ ->
     echo conn
  in
  
  Net.listen "server:1" echo >>= fun _ ->
  
  Net.connect "server:1" >>= fun cli ->

  Net.write cli msg >>= fun () -> 
  Net.read cli >|= fun got ->
  Alcotest.check (Messages.msg) "Messages are the same" msg got
  






let test_dispatch s () =
  let open Message in
  let open TMSG in 

  let cb tmsg = Lwt.return ( RMSG.create ~body:tmsg.body () ) in

  Lwt.async ( fun () -> Server.serve_dispatch "server:1" cb );

  Client.create "server:1"  >>= fun cli ->
  let req = TMSG.create ~body: (Cstruct.of_string "hello friend") () in 
  Client.dispatch cli req >|= fun rep ->
  let (exp, got) = Cstruct.to_string req.body, Cstruct.to_string rep.body in 
  Alcotest.(check string) "bodies are equal" exp got





let test_close s () =
  let open Message in
  let open TMSG in 
  
  let cb tmsg = Lwt.return ( RMSG.create ~body:tmsg.body () ) in

  let _ = Server.serve_dispatch "server:1" cb in

  Client.create "server:1"  >>= fun cli ->
  Client.create_session cli >>= fun s ->
  Session.close s 




let tests = [
  Alcotest_lwt.test_case "Test Transport" `Quick test_transport;
  

  Alcotest_lwt.test_case "Test Dispatch" `Quick test_dispatch;
  Alcotest_lwt.test_case "Test Close" `Quick test_close;
  
]

  
(*

let test_init () =
  let init = Messages.Gen.gen_init () in
  
*)
