let string length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in

    let g () = String.concat "" (Array.to_list (Array.init length gen)) in

    g


let list c gen  =
  let rec aux l =
    
    let l2 = l @ [ gen () ] in

    if (List.length l2) < c then
      aux l2

    else l2

  in

  let gen () = aux [] in
  gen


let pair ga gb =

  let gen () = 
    let a = ga () in
    let b = gb () in
    (a, b)

  in
  gen 


let cstruct size =
  let gen () = (string size) () |> Cstruct.of_string in
  gen 


let variant opts =
  let gen () =
    let i = Random.int (List.length opts) in
    List.nth opts i
  in
  
  gen ()


let repeat n f =
  let rec loop i =
    f ();
    if i > 0 then loop (i - 1)
    else ()

  in
  loop n

