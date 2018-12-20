open Base
open Stdio
;;

type coordinate = { x: int ; y: int } ;;
module Coordinate = struct
  module T = struct
    type t = coordinate
    let hash = Hashtbl.hash
    let compare t1 t2 =
      let c1 = Int.compare t1.x t2.x in
      let c2 = Int.compare t1.y t2.y in
      match c1 with
      | -1 -> -1
      | 1 -> 1
      | 0 -> c2
      | _ -> 0
    let sexp_of_t t : Sexp.t =
      List [ Int.sexp_of_t t.x ; Int.sexp_of_t t.y ]
  end
  include T
  include Comparator.Make(T)
end ;;

type direction = Down | Left | Right ;;
type tile = Clay | Flow | Water ;;

let ground_map =
  let map = Hashtbl.create (module Coordinate) in
  let input_lines = Stdio.In_channel.input_lines Stdio.stdin in
  let data_split_re = Str.regexp ", " in
  let distinguish_data ~single part_a part_b =
    if String.contains part_a '.' then
      if single then
        part_b else
        part_a
    else
    if single then
      part_a else
      part_b
  in
  let parse_range range =
    let start, finish = String.split ~on:'.' range
                  |> (fun x -> Int.of_string @@ List.hd_exn x,
                              Int.of_string @@ List.last_exn x)
    in
    let rec generate current finish =
      match current > finish with
      | true -> []
      | false -> current :: (generate (current+1) finish)
    in
    generate start finish
  in
  let fill_map map ~single ~multi =
    let single_parts = String.split single ~on:'=' in
    let single_axis = Char.of_string @@ List.hd_exn single_parts in
    let single_value = Int.of_string @@ List.last_exn single_parts in
    let multi_values = String.split multi ~on:'='
                    |> (fun x -> List.last_exn x)
                    |> parse_range
    in
    List.iter multi_values ~f:(fun multi_value ->
      match single_axis with
        | 'x' ->
          Hashtbl.set map {x= single_value ; y= multi_value } Clay
        | 'y' ->
          Hashtbl.set map {y= single_value ; x= multi_value } Clay
      | _ -> raise @@ Failure "Bad axis"
    )
  in
  let rec parse_data lines =
    match lines with
    | [] -> ()
    | line :: t ->
      let parts = Str.split data_split_re line in
      let first, second = (List.hd_exn parts), (List.last_exn parts) in
      let single = distinguish_data ~single:true first second in
      let multi = distinguish_data ~single:false first second in
      let _ = fill_map map ~single:single ~multi:multi in
      parse_data t
  in
  let _ = parse_data input_lines in
  map
;;

let y_min = Hashtbl.keys ground_map |> List.fold ~init:100000 ~f:(fun acc x -> min acc x.y) ;;
let y_max = Hashtbl.keys ground_map |> List.fold ~init:(-100) ~f:(fun acc x -> max acc x.y) ;;
let claycount map = Hashtbl.count map ~f:(fun x -> true) ;;
let clcnt = claycount ground_map ;;

let print_score map =
  let scorers1 = Hashtbl.filteri map ~f:(fun ~key:k ~data:d ->
      k.y >= y_min && k.y <= y_max && match d with | Flow | Water -> true | _ -> false
    ) in
  let _ = Stdio.printf "Part1 score %d\n" @@ List.length @@ Hashtbl.keys scorers1 in
  let scorers2 = Hashtbl.filteri map ~f:(fun ~key:k ~data:d ->
      k.y >= y_min && k.y <= y_max && match d with | Water -> true | _ -> false
    ) in
  let _ = Stdio.printf "Part2 score %d\n" @@ List.length @@ Hashtbl.keys scorers2 in
  ()
;;

let print_map map coord radius =
  (* Prints map to terminal *)
  let x_from, x_to = coord.x - radius*2, coord.x + (radius*2) in
  let y_from, y_to = coord.y - radius, coord.y + radius in
  let xs = Array.of_list @@ List.range x_from (x_to) in
  let ys = Array.of_list @@ List.range y_from y_to in
  let prev_y = ref 0 in
  let _ = Array.iter (Array.cartesian_product ys xs) ~f:(fun x_y ->
      let _ = if not (fst x_y = !prev_y) then
          let _ = Stdio.printf " %d \n" ((fst x_y)-1) in
          prev_y := fst x_y
        else
          ()
      in
      let tile = Hashtbl.find map {x= snd x_y; y= fst x_y} in
      let symbol =
        if snd x_y = coord.x && fst x_y = coord.y then
          'X'
        else
          match tile with
          | None -> '.'
          | Some s -> match s with
            | Clay -> '#'
            | Water -> '~'
            | Flow -> '*'
      in
      Stdio.printf "%c" symbol
    )
  in
  Stdio.printf "\n"
;;

let map_to_pgm map =
  (* Prints out map as PGM image *)
  let values = Hashtbl.keys map in
  let x_from = (List.fold values ~init:1000 ~f:(fun acc x -> min acc x.x)) -1 in
  let x_to = (List.fold values ~init:0 ~f:(fun acc x-> max acc (x.x))) + 1 in
  let y_from = List.fold values ~init:1000 ~f:(fun acc x -> min acc x.y) in
  let y_to = (List.fold values ~init:0 ~f:(fun acc x -> max acc (x.y))) + 1 in

  let xs = Array.of_list @@ List.range x_from x_to in
  let ys = Array.of_list @@ List.range y_from y_to in
  let cartesian = Array.cartesian_product ys xs in

  let file = Stdio.Out_channel.create "/tmp/day17.pgm" in
  let _ = Stdio.Out_channel.fprintf file "P2\n" in
  let _ = Stdio.Out_channel.fprintf file "%d %d\n" (x_to - x_from) (y_to - y_from) in
  let _ = Stdio.Out_channel.fprintf file "4\n" in

  let prev_y = ref 0 in
  Array.iter cartesian ~f:(fun point ->
      let y, x = point in
      let _ = if !prev_y = 0 then
          prev_y := y
        else
          ()
      in
      let _ = if not (!prev_y = y) then
          let _ = Stdio.Out_channel.fprintf file "\n" in
          prev_y := y
      in
      let tile = Hashtbl.find map {x=x; y=y} in
      let symbol = match tile with
        | None -> '1'
        | Some t -> match t with
          | Flow -> '3'
          | Water -> '2'
          | Clay -> '0'
      in
      Stdio.Out_channel.fprintf file "%c " symbol
    )
;;

let check_move map coord =
  if coord.y <= y_max then
    coord |> Hashtbl.find map
  else
    Some Flow
;;

let move_coordinate ~dir coordinate =
  match dir with
  | Down -> {coordinate with y = coordinate.y+1}
  | Left -> {coordinate with x = coordinate.x-1}
  | Right -> {coordinate with x = coordinate.x+1}
;;

let tile_blocked map coord ~dir =
  Hashtbl.mem map (move_coordinate ~dir:dir coord)
;;

let make_layer_flow map coord =
  let _ = Stdio.printf "Flowing layer on %d,%d\n" coord.x coord.y in
  let rec step ~xstep coord =
    Hashtbl.find map coord
    |> (fun x -> match x with
        | None -> ()
        | Some tile -> match tile with
          | Water | Flow ->
            let _ = Hashtbl.set map coord Flow in
            step ~xstep:xstep {coord with x = coord.x + xstep}
          | _ -> ()
      )
  in
  let _ = Hashtbl.set map coord Flow in
  let _ = step (-1) coord in
  let _ = step (1) coord in
  ()
;;

let dir_repr dir =
  match dir with
  | Left -> "Left"
  | Right -> "Right"
  | Down -> "Down"

let tile_repr tile =
  match tile with
  | Some Clay -> "Clay"
  | Some Water -> "Water"
  | Some Flow -> "Flow"
  | None -> "None"

let rec process_streams2 map streams =
  match streams with
  | [] -> None
  | stream :: t ->
    let movedir, coordinate = stream in
    let bumped_into = check_move map coordinate in
    match bumped_into with
    | Some tile -> Some tile
    | None ->
      let _ = match movedir with
        | Down -> Hashtbl.set map coordinate Flow
        | Left | Right -> Hashtbl.set map coordinate Water
      in
      (* let _ = print_map map {x= coordinate.x; y=coordinate.y} 10 in *)

      let moved_coordinate = move_coordinate ~dir:movedir coordinate in
      let moved_stream = if tile_blocked map moved_coordinate ~dir:Down then
        movedir, moved_coordinate
      else
        Down, moved_coordinate
      in

      let move_result = process_streams2 map (moved_stream :: t) in
      match move_result with
      | None -> None
      | Some Flow ->
        Some Flow
      | Some Water | Some Clay ->
        match movedir with
        | Right | Left -> move_result
        | Down ->
          let _ = Hashtbl.set map coordinate Water in
          let stream = Left, move_coordinate ~dir:Left coordinate in
          let res_left = process_streams2 map (stream :: t) in
          let stream = Right, move_coordinate ~dir:Right coordinate in
          let res_right = process_streams2 map (stream :: t) in
          match res_left, res_right with
          | Some Flow, _ | _, Some Flow ->
            let _ = make_layer_flow map coordinate in
            Some Flow
          | _ ->
            let _ = Hashtbl.set map coordinate Water in
            Some Water
;;

let streams = [ (Down, {x= 500 ; y= 0}) ] in
let _ = process_streams2 ground_map streams in
let _ = print_score ground_map in
let _ = map_to_pgm ground_map in
0

