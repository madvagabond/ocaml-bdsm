module HC = Codec.HeadersCodec
module PC = Codec.PathCodec



module TMSG = struct
    
  type t = {
      path: string list;
      headers: (string * string) list;
      body: Cstruct.t 
    } [@@deriving fields]



  let path_default = []
                       
  let headers_default = []
                          
  let body_default = Cstruct.empty


  let make
        ?path:(path=path_default)
        ?headers:(headers=headers_default)
        ?body:(body=body_default)
        () =

    {path; headers; body}



      
  let decode buf =
    
    let mbuf = Mstruct.of_cstruct buf in

    let path_r = PC.decode mbuf in

    let hdr = HC.decode mbuf in

    let body_r = Codec.read_field mbuf in

    match (path_r, hdr, body_r) with
    | (
         Ok(path), Ok(headers),  Ok(body)
      ) ->
       let req = {path; headers; body} in
       Ok req

 
    | _ -> Error "unable to parse tmsg"
                 
       


  let encode t =
    let bufs =
      [
        PC.encode (t.path);

        HC.encode (t.headers);

        Codec.write_field (t.body); 
      ]
    in


    Cstruct.concat bufs 


      
end







                

module RMSG = struct
    
  type t = {
      headers: (string * string) list;
      body: Cstruct.t
    } [@@deriving fields]




  let headers_default = []
                          
  let body_default = Cstruct.empty

      
  let make
        ?headers:(headers=headers_default)
        ?body:(body=body_default)
        () =
    {headers; body}


      


  let decode buf =
    
    let mbuf = Mstruct.of_cstruct buf in
    
    
    let hdr = HC.decode mbuf in


    let body_r = Codec.read_field mbuf in

    match (hdr, body_r) with
    | ( Ok(headers),  Ok(body)  ) ->
       let req = {headers; body} in
       Ok req

          
    | _ -> Error "unable to parse tmsg"



                 
  let encode t =
    let bufs =
      [
        HC.encode (t.headers );
        Codec.write_field (t.body); 
      ]
    in


    Cstruct.concat bufs 
                   
                   
                   

        

end 
