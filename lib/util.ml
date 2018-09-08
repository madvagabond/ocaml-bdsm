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


module Inbox = struct
  type 'a t = {
      lock: Lwt_mutex.t; 
      tbl: (int32, 'a) Hashtbl.t
    }

  let rec remove_all tbl key =
    if Hashtbl.mem tbl key then begin
        Hashtbl.remove tbl key;
        remove_all tbl key
      end
                                  


  let shutdown t tag =
    Lwt_mutex.with_lock t.lock (fun () ->
        Hashtbl.remove t.tbl tag;
        Lwt.return_unit 
      )




  let create t tag a =
    if (Hashtbl.mem t.tbl tag) then
      Error "session already exists" |> Lwt.return 
    else
      Lwt_mutex.with_lock t.lock ( fun () ->
          Hashtbl.add t.tbl tag a;
          Lwt.return (Ok a)
        )
        



  let map t id =
      Lwt_mutex.with_lock t.lock (fun () ->
        let r = result_of_opt (Hashtbl.find_opt t.tbl id) in
        Lwt.return r
      )





  let unmap t id =
    Lwt_mutex.with_lock t.lock (fun () ->
        Hashtbl.find_opt t.tbl id |>
        function
        | Some a ->
          Hashtbl.remove t.tbl id;
          Ok a |> Lwt.return

        | None ->
          
          Error "doesn't exist" |> Lwt.return
      )



(*
  let register t tag =
     




  
  let map t tag a =
    
    Lwt_mutex.with_lock t.lock ( fun () ->
        let mbox = Hashtbl.find_opt t.tbl tag in
        match mbox with
        | Some mbox ->
          Lwt_mvar.put mbox a >|= fun () -> Ok ()

        | None -> Error "Not_found" |> Lwt.return 
                                
      )
  
                   
*)

    
end
