module type Chan = sig
  type flow
  type t

                                   
  val read: flow -> (t, 'a) result Lwt.t
  val write: flow -> t -> unit Lwt.t
                               
end


                     
module type Net = sig

  

  module Flow: Mirage_flow_lwt.S
  type flow
  type ctx
         

         
  val connect: ctx -> string -> int -> flow Lwt.t 
  val close: flow -> unit Lwt.t 
                          
  val default_ctx: unit -> ctx
                     
                        
  
end






module type RMSG = sig

  type t

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

  val headers: t -> Headers.t
  val body: t -> Cstruct.t



  module Chan: Chan with type t = t
                                
end


                  
module type TMSG = sig


  
  type t


      
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

  val body: t -> Cstruct.t
  val headers: t -> Headers.t

  val path: t -> string list
                        

  module Chan: Chan with type t = t
                                
          
end


module type Transport = sig
  type flow
         
  module TMSG: TMSG with type Chan.flow = flow
  module RMSG: RMSG with type Chan.flow = flow 


  val read_rmsg: flow -> (RMSG.t, 'a) result Lwt.t
  val read_tmsg: flow -> (TMSG.t, 'a) result Lwt.t

  val write_tmsg: flow -> TMSG.t -> unit Lwt.t 
  val write_rmsg: flow -> RMSG.t -> unit Lwt.t
         
end 



                     
module type Client = sig
  include Net 
  include Transport with type flow := flow
            
            

  val use: flow -> TMSG.t -> RMSG.t Lwt.t
             
                              
                                                             
end

                       

module type Listener = sig
  include Mirage_flow_lwt.S 
  val listen: ?host: string -> port:int -> (flow -> unit Lwt.t) -> unit Lwt.t
                                                     
end

module type Server = sig
  module Flow: Mirage_flow_lwt.S

                
  type flow = Flow.flow
                
  include Transport with type flow := flow
                                       
  type cb = (flow * TMSG.t) -> RMSG.t Lwt.t

                                     

  val listen: ?host: string -> port:int -> (flow -> unit Lwt.t) -> unit Lwt.t


                                                     
  val serve_callback: ?host:string -> port: int -> cb -> unit Lwt.t

                                                              

  val serve_raw: ?host:string -> port:int -> (flow * TMSG.t -> unit Lwt.t)
                                               
                                                     
                                                                      
end
