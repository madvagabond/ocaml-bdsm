open Util
       
       
type t = (string * string) list


let map_option o f =
  match o with
  | Some x -> Some ( f x )
  | None -> None
  
                           
let get t key =
  let o = List.find_opt (fun (k, _) ->  k = key ) t in
  map_option o (fun (_, v) -> v) 

                  

let get_multi t key =
  List.fold_left
    ( fun acc (k, v) ->
      
      if k = key then acc @ [v]
      else acc
    ) [] t


    
let add t k v =
  t @ [(k, v)]



        
let add_multi t key vals =
  let entries = List.map (fun x -> (key, x) ) vals in 
  t @ entries
        


        
let add_list t entries =
  t @ entries



        
let get_list t list =
  List.fold_left
    (
      fun acc (k, v)->

      if (List.exists (fun key -> key = k) list) then
        acc @ [(k, v)]

      else acc
    ) [] t


    

let init () = []

let remove t key =
  List.filter (fun (k, _) -> k <> key ) t

                 

let buffer_size t =
   List.fold_left (fun acc (k, v) ->
        acc + 4 + (String.length k) + (String.length v)
     ) 2 t 






let write t buf =
  let count = List.length t in
  let buf1 = write count 2 Cstruct.BE.set_uint16 buf in
  
    List.fold_left (fun acc kv ->
        let (k, v) = kv in
        let ki, vi = String.length k, String.length v in

        write ki 2 Cstruct.BE.set_uint16 acc
        |> write_string k
        |> write vi 2 Cstruct.BE.set_uint16
        |> write_string v
      ) buf1 t


  


    
let read buf =
  let (count, rest) = Util.read 2 (Cstruct.BE.get_uint16) buf in

  let rec aux ct b hdrs =
    if (ct >= 1) then
      let (ki, rest) = read 2 (Cstruct.BE.get_uint16) b in
      let (k, rest0) = read_string ki rest in

      let (vi, rest1) = read 2 (Cstruct.BE.get_uint16) rest0 in
      let (v, rest2) = read_string vi rest1 in

      let h = hdrs @ [(k, v)] in

      aux (ct - 1) rest2 h

    else
      hdrs, b

  in
  
  aux count rest []

    


let to_cstruct t =
  let bsize = buffer_size t in
  let buf = Cstruct.create bsize in
  let _ = write t buf in
  buf 

    

let of_cstruct buf =
  let (hdrs, _) = read buf in
  hdrs
