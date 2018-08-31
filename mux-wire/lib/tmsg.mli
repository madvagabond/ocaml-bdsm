type tmsg = {
    path: string list;
    headers: Mux.Headers.t;
    body: Cstruct.t 
  }
              
include Mux.S.TMSG with type t = tmsg


                                   
