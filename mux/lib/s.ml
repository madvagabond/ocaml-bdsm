module type Chan = sig
  type flow
  type t

                                   
  val read: flow -> (t, 'a) result Lwt.t
  val write: flow -> t -> unit Lwt.t
                               
end


                     
module type Net = sig

  

  include Mirage_flow_lwt.S
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

  val add_headers: t -> Headers.t -> t

  val headers: t -> Headers.t
  val body: t -> Cstruct.t



  module IO: functor (F: Mirage_flow_lwt.S) -> Chan with type t = t and type flow = F.flow
                                
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

                                       
  val add_headers: t -> (Headers.t -> Headers.t) -> t
  val set_headers: t -> Headers.t  -> t

  val body: t -> Cstruct.t
  val headers: t -> Headers.t

  val path: t -> string list

  module IO: functor (F: Mirage_flow_lwt.S) -> Chan with type t = t and type flow = F.flow
                        

  
                                
          
end


module type Transport = sig
  module F: Mirage_flow_lwt.S 

  module TMSG: TMSG
  module RMSG: RMSG
                 

  module TCH: Chan with type t = TMSG.t and type flow = F.flow
  module RCH: Chan with type t = RMSG.t and type flow = F.flow
                                    
         
  val read_rmsg: F.flow -> (RMSG.t, 'a) result Lwt.t
  val read_tmsg: F.flow -> (TMSG.t, 'a) result Lwt.t

  val write_tmsg: F.flow -> TMSG.t -> unit Lwt.t 
  val write_rmsg: F.flow -> RMSG.t -> unit Lwt.t
         
end 



                     
module type Client = sig
  include Net 
  include Transport with type F.flow = flow
            
            

  val use: flow -> TMSG.t -> RMSG.t Lwt.t
             
                              
                                                             
end

                       

module type Listener = sig
  include Mirage_flow_lwt.S 
  val listen: ?host: string -> port:int -> (flow -> unit Lwt.t) -> unit Lwt.t
                                                     
end

module type Server = sig
  module Flow: Mirage_flow_lwt.S

                
  type flow = Flow.flow
                
  include Transport
                                       
  type cb = (flow * TMSG.t) -> RMSG.t Lwt.t

                                     

  val listen: ?host: string -> port:int -> (flow -> unit Lwt.t) -> unit Lwt.t
                             
  val serve_callback: ?host:string -> port: int -> cb -> unit Lwt.t                                                           

  val serve_raw: ?host:string -> port:int -> (flow * TMSG.t -> unit Lwt.t)
                                               
                                                     
                                                                      
end
