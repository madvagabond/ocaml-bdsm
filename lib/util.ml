open Lwt.Infix
       
let write c len f t =
  f t 0 c;
  Cstruct.shift t len 
  

let read len f t =
  let got = f t 0  in
  let rest = Cstruct.shift t len in
  (got, rest)
  


let write_string str t=
  
  let len = String.length str in
  Cstruct.blit_from_string str 0 t 0 len;
  Cstruct.shift t len 
  

let read_string len t =
  let got = Cstruct.copy t 0 len in
  got, (Cstruct.shift t len)



let result_of_opt o =
  match o with
  | Some x -> Ok x
  | None -> Error "Empty"
              

let map_result res f =
  match res with
  | Ok x -> Ok (f x)
  | Error e -> Error e 


let fmap_result res f =
  match res with
  | Ok x -> f x
  | Error x -> Error x 

            

let get_result res =
  match res with
  | Ok x -> x
  | Error e -> raise (Invalid_argument e)


module Tbl = struct
  include Hashtbl

    let rec remove_all tbl key =
    if Hashtbl.mem tbl key then begin
        Hashtbl.remove tbl key;
        remove_all tbl key
      end
end
