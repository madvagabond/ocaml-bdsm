module TMSG: sig


  
  type t = {
      path: string list;
      headers: Headers.t;
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

                                       
  val with_headers: t -> (Headers.t -> Headers.t) -> t
  val set_headers: t -> Headers.t  -> t

                                        
  val with_body: t -> (Cstruct.t -> Cstruct.t) -> t
  val set_body: t -> Cstruct.t -> t 
                     
          
end




module RMSG: sig

  
  type t = {
      headers: Headers.t;
      body: Cstruct.t
    } [@@deriving fields]


  val make:
    ?headers: (string * string) list ->
    ?body: Cstruct.t ->
    unit ->
    t



  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> (t, string) result

  val with_headers: t -> (Headers.t -> Headers.t) -> t
  val set_headers: t -> Headers.t  -> t


  val with_body: t -> (Cstruct.t -> Cstruct.t) -> t
  val set_body: t -> Cstruct.t -> t 
                                        
             
end 
