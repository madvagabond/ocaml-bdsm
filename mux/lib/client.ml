open S
open Lwt.Infix
       





module Make (N: Net) (TMSG: TMSG with type Chan.flow = N.flow) (RMSG: RMSG with type Chan.flow = N.flow): Client = struct

  

  include N
            
  module TMSG = TMSG
  module RMSG = RMSG
                  

            
  let read_rmsg flow =
    RMSG.Chan.read flow

  let read_tmsg flow =
    TMSG.Chan.read flow

  let write_tmsg flow tmsg =
    TMSG.Chan.write flow tmsg

  let write_rmsg flow rmsg =
    RMSG.Chan.write flow rmsg





  let use flow tmsg =
    write_tmsg flow tmsg >>= fun _ ->
    read_rmsg flow >>= fun res ->

    match res with
    | Ok rep -> Lwt.return rep
    | Error _ ->
       close flow >>= fun _ -> 
       Lwt.fail_with "Failed to read response"
                     
                     
end
