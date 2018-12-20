open Base
open Stdio
;;

type xy = int * int ;;
type cells = Ground | Tree | Yard ;;

let input_lines = Stdio.In_channel.input_lines Stdio.stdin ;;
let width, height = String.length (List.hd_exn input_lines), List.length input_lines ;;

let set_field field coord cell =
  Array.set field.(snd coord) (fst coord) cell
;;

let get_field field coord =
  try
    Some field.(snd coord).(fst coord)
  with Invalid_argument _ -> None
;;

let iteri_field field func =
  Array.iteri field ~f:(fun i_y row ->
      Array.iteri row ~f:(fun i_x cell ->
          func (i_x, i_y) cell
        )
    )
;;

let count_adjacents field coord =
  let adjacents = [
    get_field field ( (fst coord), (snd coord-1) ) ;
    get_field field ( (fst coord+1), (snd coord-1) ) ;
    get_field field ( (fst coord+1), (snd coord) ) ;
    get_field field ( (fst coord+1), (snd coord+1) ) ;
    get_field field ( (fst coord), (snd coord+1) ) ;
    get_field field ( (fst coord-1), (snd coord+1) ) ;
    get_field field ( (fst coord-1), (snd coord) ) ;
    get_field field ( (fst coord-1), (snd coord-1) ) ;
  ] in
  let trees = List.count adjacents ~f:(fun cell -> match cell with Some Tree -> true | _ -> false) in
  let yards = List.count adjacents ~f:(fun cell -> match cell with Some Yard -> true | _ -> false) in
  trees, yards
;;

let initial_field =
  let data = Array.make_matrix ~dimx:width ~dimy:height Ground in
  let _ = List.iteri input_lines ~f:(fun i_y line ->
      List.iteri (String.to_list line) ~f:(fun i_x char ->
          set_field data (i_x, i_y) (match char with
          | '.' -> Ground
          | '|' -> Tree
          | '#' -> Yard
          | _ -> raise @@ Failure "Bad token"
        ))
    )
  in
  data
;;

let calc_score field =
  let trees = ref 0 in
  let yards = ref 0 in
  let _ = iteri_field field (fun xy cell ->
      match cell with
      | Tree -> trees := !trees + 1
      | Yard -> yards := !yards + 1
      | _ -> ()
    )
  in
  let score = (!trees * !yards) in
  score
;;

let print_field field =
  let last_y = ref 0 in
  let _ = iteri_field field (fun xy cell ->
      let _ = if not ((snd xy) = !last_y) then
        let _ = last_y := (snd xy) in
        Stdio.printf "\n"
      in
      Stdio.printf "%c" (match cell with
      | Tree -> '|'
      | Yard -> '#'
      | Ground -> '.'
      )
    )
  in
  Stdio.printf "\n\n"
;;

let deepcopy_field field =
  let data = Array.make_matrix ~dimx:width ~dimy:height Ground in
  let _ = iteri_field field (fun xy cell ->
      set_field data xy cell
    )
  in
  data
;;

let morph_cell field coord =
  let contents = get_field field coord in
  let nr_trees, nr_yards = count_adjacents field coord in
  match contents, nr_trees, nr_yards with
  | Some Ground, trees, _ when trees >= 3 -> Some Tree
  | Some Tree, _, yards when yards >= 3 -> Some Yard
  | Some Yard, trees, yards when trees >= 1 && yards >= 1 -> Some Yard
  | Some Yard, _, _ -> Some Ground
  | _ -> None
;;


let score_memory = Hashtbl.create (module Int) ~size:1000 in
let score_manager iteration score part1 part2 prev_collision =
  if part1 = 0 then
    let _ = Stdio.printf "Part1: %d\n" score in
    prev_collision
  else
    let score_seen = Hashtbl.find score_memory score in
    match score_seen with
    | None ->
      let _ = Hashtbl.set score_memory score iteration in
      prev_collision
    | Some iter ->
      let collision = iteration - iter in
      let _ = Hashtbl.set score_memory score iteration in
      if collision = prev_collision && prev_collision > 0 && (part2 % prev_collision = 0) then
        let _ = Stdio.printf "Part2: %d\n" score in
        Caml.exit 0
      else
        collision
in
let rec mainloop iteration part1 part2 old_field ~prev_collision =
  let score = calc_score old_field in
  let prev_collision = score_manager iteration score part1 part2 prev_collision in
  let new_field = deepcopy_field old_field in
  let _ = iteri_field old_field (fun xy cell ->
      match morph_cell old_field xy with
      | Some new_cell -> set_field new_field xy new_cell
      | None -> ()
    )
  in
  mainloop (iteration+1) (part1-1) (part2-1) new_field ~prev_collision:prev_collision
in
let part1 = 10 in
let part2 = 1000000000 in
mainloop 0 part1 part2 initial_field ~prev_collision:0


