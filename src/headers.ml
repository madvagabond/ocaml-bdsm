type t = (string * string) list



                           
let get t key =
  let o = Base.List.find t (fun (k, _) ->  k = key ) in
  Base.Option.map o (fun (k, _) -> k) 

                  

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
