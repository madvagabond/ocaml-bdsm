open S

module Make (FLOW: Mirage_flow_lwt.S) (TMSG: TMSG) (RMSG: RMSG):
Transport with type F.flow = FLOW.flow = struct

  module F = FLOW

  module TMSG = TMSG
  module RMSG = RMSG
                  
  
  module TCH = TMSG.IO(FLOW)
  module RCH= RMSG.IO(FLOW)

  let read_rmsg flow =
    RCH.read flow

  let read_tmsg flow =
    TCH.read flow

  let write_tmsg flow tmsg =
    TCH.write flow tmsg

  let write_rmsg flow rmsg =
    RCH.write flow rmsg

                      
                      
end
