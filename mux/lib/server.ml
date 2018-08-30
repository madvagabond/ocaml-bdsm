open S
open Lwt.Infix
       
       
module Make(L: Listener)(TMSG: TMSG)(RMSG: RMSG) = struct




  module Flow = L
                  
            
  include Transport.Make(L)(TMSG)(RMSG)
                        
          (*        
  let read_rmsg flow =
    RMSG.Chan.read flow

  let read_tmsg flow =
    TMSG.Chan.read flow

  let write_tmsg flow tmsg =
    TMSG.Chan.write flow tmsg

  let write_rmsg flow rmsg =
    RMSG.Chan.write flow rmsg

*)


  
  let listen ?host ~port cb =
    L.listen ?host ~port cb 
                                      

  let serve_raw ?host ~port cb =
    let cb1 flow =
      read_tmsg flow >>= fun res ->
      match res with
        
      | Ok tmsg -> cb flow tmsg
                       
      | Error _ ->
         Flow.close flow >>= fun _ -> 
         Lwt.fail_with "Unable to read request"
    in

    listen ?host ~port cb1
           

           
  let serve_callback ?host ~port cb =
    let cb1 flow tmsg =
      let rep = cb flow tmsg in
      write_rmsg flow rep 
    in
    
    serve_raw ?host ~port cb1

    
    
      
            
end
