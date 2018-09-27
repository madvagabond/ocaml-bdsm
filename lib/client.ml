open Util
open Lwt.Infix
       

module Make (C: S.Connector): S.Endpoint = struct

  module SM = SessionManager.Make(C)

  type peer = C.peer
  type conn = C.conn


  type t = {
    trackers: (peer, SM.t) Tbl.t    
  }


  

  let find_sm t peer =
    let o = Tbl.find_opt t.trackers peer in
    
    match o with
    | Some x ->
      Lwt.return x
   
    | None ->
      C.connect peer >|= fun c -> 
      SM.create c 


  
  let create_session t peer =
    let o = Tbl.find_opt t.trackers peer in
    
    match o with
    | Some x ->
      SM.create_session x

    | None ->
      C.connect peer >>= fun c -> 
      let sm = SM.create c in 

      SM.create_session sm 
      


    


  let serve t peer ~cb () =
    find_sm t peer >>= fun sm ->
    
    let _ = SM.read_loop sm ~cb () in
    let _ = SM.write_loop sm  in
    Lwt.return_unit


  let shutdown t peer =
    Tbl.find_opt t.trackers peer |>
    function
    | Some x -> SM.shutdown x

    | None -> Lwt.return_unit
    



  let ping t peer =
    find_sm t peer >>= fun sm ->
    SM.ping sm

  

end
