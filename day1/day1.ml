
let day1 =
  let rec readlines got =
    try
      let line = read_line () in
      readlines @@ line :: got
    with End_of_file -> List.rev got in
  let inplines = readlines [] in
  let dupmap = Hashtbl.create 1000 in
  let rec parseline acc lines =
    match lines with
    | [] -> acc 
    | h :: t -> 
      let inpnum = int_of_string h in
      let newnum = acc + inpnum in
      let isdup = Hashtbl.mem dupmap newnum in
      let _ = Hashtbl.add dupmap newnum 0 in
      let _ = Printf.printf "%d + %d -> %d duplicate: %b\n" acc inpnum newnum isdup in
      parseline newnum t
  in
  let lastaccum = ref 0 in
  let rec loopinput count =
    match count with
    | 0 -> ()
    | _ ->
      lastaccum := parseline (!lastaccum) inplines;
      loopinput (count - 1)
  in
  (* How many times to loop max *)
  let _ = loopinput 200
  in ()
;;

let _ = day1
