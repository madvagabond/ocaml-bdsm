open Util

module Make (L: S.Listener): S.Endpoint = struct

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

    | None -> Lwt.fail_with "no such peer is connected"




  
  
end

