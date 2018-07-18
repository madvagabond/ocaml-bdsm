module TMSG: sig


  
  type t = {
      path: string list;
      headers: (string * string) list;
      body: Cstruct.t 
    } [@@deriving fields]



      
  val make:
    ?path: string list ->
    ?headers: (string * string) list ->
    ?body: Cstruct.t ->
    unit -> 
    t

  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> (t, string) result
                                       
                     
          
end




module RMSG: sig

  
  type t = {
      headers: (string * string) list;
      body: Cstruct.t
    } [@@deriving fields]


  val make:
    ?headers: (string * string) list ->
    ?body: Cstruct.t ->
    unit ->
    t



  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> (t, string) result
             
end 
