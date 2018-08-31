type rmsg = {
    headers: (string * string) list;
    body: Cstruct.t
  }

include Mux.S.RMSG with type t = rmsg
                                   
