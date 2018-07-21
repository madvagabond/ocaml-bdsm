open Base.Result
open Lwt.Infix


                      
let reset buf =
  let pos = Mstruct.offset buf |> fun x -> (0 - x) in
  Mstruct.shift buf pos;
  buf
              
  

let parse_result f =
  try Ok ( f () )
         
  with e ->
    let err_s  = Printexc.to_string e in
    Error err_s 
          

let read_field buf =
  let f () =
    let i = Mstruct.get_be_uint32 buf |> Int32.to_int in 
    let o = Mstruct.sub buf 0 i |> Mstruct.to_cstruct in

    Mstruct.shift buf i;
    o           
  in

  parse_result f


                               



    
    
let write_field field =    
  let buf = Cstruct.create 4 in 
  let i = Cstruct.len field |> Int32.of_int in
  Cstruct.BE.set_uint32 buf 0 i;

  Cstruct.append buf field


let write_string s =
  Cstruct.of_string s |> write_field

let read_string buf =
  read_field buf >>| fun x ->
  Cstruct.to_string x

                           


module type Codec = sig
  type t 
  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> (t, string) result 
end 


module type StringField = sig
  type t 


  val from_string: string -> t
  val to_string: t -> string 
                        
end

                            

                            
module type DelimitedField = sig
  include StringField
  val delim: string
end


                               
                               
module DelimitedList(SE: DelimitedField) = struct
  type t = SE.t list
                
  let tok = SE.delim
              
              

  let to_string t =
    let base = "" in
    
    List.fold_left
      ( fun acc x ->
        let a = SE.to_string x in
        Fmt.strf "%s%s%s" acc a tok
      ) base t


      

  let from_string s =
    
    let segs = Str.split (Str.regexp tok ) s in
    List.map (fun x -> SE.from_string x) segs

             
end

                                            

module StringCodec (S: StringField) = struct
  include S
            
  let decode buf =
    read_string buf >>| fun x ->
    S.from_string x

  let encode t =
    S.to_string t |> write_string
end




                                        
                   
module Path = struct

  type t = string list
                  
  let from_string s =
    Str.split (Str.regexp "/") s

  let to_string t =
    String.concat "/" t 
end

                

module Header = struct

  type t = (string * string)

  let from_string x =
    let tokens =  Str.split (Str.regexp ":") x in


    let hd :: tl = 
      if (List.length tokens > 0 ) then tokens
      else ["";""]
    in
    
    
    

    let v = String.concat ":" tl in
    (hd, v)



  let to_string t =
    let (k, v) = t in
    Fmt.strf "%s:%s" k v

  let delim = "\n"
                



                
end 


                  
module Headers = DelimitedList(Header) 

module HeadersCodec = StringCodec(Headers)
module PathCodec = StringCodec(Path)


 

module Reader (Flow: Mirage_flow_lwt.S) (C: Codec) = struct


  let too_big =
    let msg = "message exceeded buffer size of 100KB" in
    Cstruct.of_string msg



  let read flow =

    let rec aux buf =
 
      
      Flow.read flow >>= fun read_res ->


      match read_res with

      | Ok (`Data data) ->
         let buf1 = Cstruct.append buf data in

         begin 

           if (Cstruct.len buf1) <= 100000 then


             let res = C.decode buf1 in

             if (Base.Result.is_ok res) then Lwt.return res

             else aux buf1 


           else
             Flow.write flow too_big >|= fun _ ->
             Error ( "Message exceeded size" )

         end


      | Ok `Eof ->
         Flow.close flow >|= fun () ->
         Error "Client closed connection before message could be decoded"

      | Error e ->
         let emsg = Fmt.strf "%a" (Flow.pp_error) e in
         Error emsg |> Lwt.return 


    in

    let buf = Cstruct.empty in
    aux buf                      






end
