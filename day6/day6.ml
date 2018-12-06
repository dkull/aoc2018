exception BadData of string

let load_data_and_parse =
  let rec read_input got =
    try
      let line = read_line () in
      read_input (line :: got)
    with End_of_file -> got
  in
  let parse_line line =
    line |> String.split_on_char ',' |> List.map String.trim
    |> List.map int_of_string
    |> fun x -> (List.hd x, List.hd @@ List.tl x)
  in
  List.map parse_line (read_input [])

let inc_hashtbl tbl key =
  let curval = match Hashtbl.find_opt tbl key with None -> 0 | Some x -> x in
  Hashtbl.replace tbl key @@ (curval + 1)

let manhattan a b = abs (fst a - fst b) + abs (snd a - snd b)

let is_at_edge f t c =
  fst c = fst f || fst c = fst t || snd c = snd f || snd c = snd t

let walk_the_grid f t points =
  let width, height = (fst t - fst f, snd t - snd f) in
  let point_cells = Hashtbl.create @@ List.length points in
  let bad_points = Hashtbl.create @@ List.length points in
  let over10k = ref 0 in
  let rec walker nth =
    let x, y = (fst f + (nth mod (width + 1)), snd f + (nth / height)) in
    if y > snd t then ()
    else
      let distances = List.map (manhattan (x, y)) points in
      let point_and_dist = List.combine points distances in
      (* Part 2 *)
      let distance_sum = List.fold_left ( + ) 0 distances in
      let _ = if distance_sum < 10000 then over10k := !over10k + 1 else () in
      let by_closest =
        List.sort (fun x y -> compare (snd x) (snd y)) point_and_dist
      in
      let first, second =
        (List.hd by_closest, List.hd @@ List.tl by_closest)
      in
      let _ =
        if snd first == snd second then ()
        else if is_at_edge f t (x, y) then
          Hashtbl.replace bad_points (fst first) (-1)
        else inc_hashtbl point_cells (fst first)
      in
      walker (nth + 1)
  in
  let _ = walker 0 in
  let _ = Hashtbl.replace_seq point_cells (Hashtbl.to_seq bad_points) in
  let _ = Printf.printf "Over 10K: %d\n" !over10k in
  point_cells

;;
let data = load_data_and_parse in
let xy_from, xy_to =
  let xs, ys = List.split data in
  ( ( List.fold_left min (List.hd xs) (List.tl xs)
    , List.fold_left min (List.hd ys) (List.tl ys) )
  , ( List.fold_left max (List.hd xs) (List.tl xs)
    , List.fold_left max (List.hd ys) (List.tl ys) ) )
in
let point_cells = walk_the_grid xy_from xy_to data in
let sorted_counts =
  List.sort (fun x y -> compare (snd x) (snd y))
  @@ List.of_seq @@ Hashtbl.to_seq point_cells
in
List.map
  (fun x -> Printf.printf "%d,%d %d\n" (fst (fst x)) (snd (fst x)) (snd x))
  sorted_counts
