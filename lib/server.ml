open Util
open Lwt.Infix


module Make (L: S.Listener): S.Server with type peer := L.peer and type conn := L.conn  = struct

  
  
  type peer = L.peer

  type conn = L.conn


    
  module SM = SessionManager.Make(L)

  

  type t = {
    trackers: (peer, SM.t) Util.Tbl.t
  }

  
  let shutdown t peer =
    let mgr = Tbl.find t.trackers peer in
    SM.shutdown mgr 


  
  let ping t peer =
    let mgr = Tbl.find t.trackers peer in
    SM.ping mgr


  
  let create_session t peer =
    let sm = Tbl.find_opt t.trackers peer in
    match sm with
    | Some sm ->
      SM.create_session sm

    | None ->
      Lwt.fail_with "no such peer is connected"



  let create () =
    {trackers = Hashtbl.create 20000 }


  
  let serve_cb t cb conn =
    let dst = L.dst conn in
    let sm = SM.create conn in

    let _ = SM.read_loop sm ~cb () in
    let _ = SM.write_loop sm  in
    
    let _ = Tbl.add t.trackers dst sm in 
    Lwt.return_unit





  let serve peer callback =
    let t = create () in
    let cb = serve_cb t callback in
    let _ = L.listen peer cb in
    Lwt.return t
    
    

  let serve_dispatch peer callback =
    let cb s =  Session.serve_dispatch s callback in
    serve peer cb

end


(*
module Make (L: S.Listener) = struct

  
  type peer = L.peer

  type conn = L.conn

  module Listener = L
    
  module SM = SessionManager.Make(Listener)

  

  type t = {
    trackers: (peer, SM.t) Util.Tbl.t
  }


  let serve_cb t cb conn =
    let sm = SM.create conn in
    let _ = SM.read_loop sm ~cb () in
    let _ = SM.write_loop sm  in
    Lwt.return_unit


  


  let serve t peer ~cb () =
    let callback = (serve_cb t cb) in  
    Listener.listen peer callback



  let shutdown t peer =
    let mgr = Tbl.find t.trackers peer in
    SM.shutdown mgr 


  
  let ping t peer =
    let mgr = Tbl.find t.trackers peer in
    SM.ping mgr


  
  let create_session t peer =
    let sm = Tbl.find_opt t.trackers peer in
    match sm with
    | Some sm ->
      SM.create_session sm

    | None ->
      Lwt.fail_with "no such peer is connected"




  
  
end

*)
