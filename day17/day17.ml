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

type direction = Left | Right ;;
type tile = Clay | Flow | Water ;;

let ground_map =
  let map = Hashtbl.create (module Coordinate) ~size:10000 in
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

let calc_score map =
  let scorers = Hashtbl.filteri map ~f:(fun ~key:k ~data:d ->
      k.y >= y_min && k.y <= y_max && match d with | Flow | Water -> true | _ -> false
    ) in
  let _ = Hashtbl.iteri scorers ~f:(fun ~key:k ~data:v ->
      Hashtbl.set map ~key:k ~data:Water
    )
  in
  List.length @@ Hashtbl.keys scorers
;;


let print_map map coord radius =
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
  let values = Hashtbl.keys map in
  let x_from = List.fold values ~init:1000 ~f:(fun acc x -> min acc x.x) in
  let x_to = List.fold values ~init:0 ~f:(fun acc x-> max acc (x.x + 5)) in
  let y_from = List.fold values ~init:1000 ~f:(fun acc x -> min acc x.y) in
  let y_to = List.fold values ~init:0 ~f:(fun acc x -> max acc (x.y + 5)) in

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
  coord
  |> Hashtbl.find map
  |> (fun tile -> match tile with
      | None -> Ok coord, None
      | Some tile -> match tile with
        | Clay | Water | Flow-> Error coord, Some tile
    )
;;

let move_stream_down map coord =
  let new_coord = { coord with y = coord.y + 1} in
  new_coord |> check_move map
;;

let move_stream_sideways ~dir map coord =
  let new_coord = match dir with
    | Left -> { coord with x = coord.x - 1}
    | Right -> { coord with x = coord.x + 1}
  in
  new_coord |> check_move map
;;

let make_layer_flow map coord =
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
  let _ = step (-1) coord in
  let _ = step (1) coord in
  ()
;;

let max_y = Hashtbl.keys ground_map
          |> List.map ~f:(fun coord -> coord.y)
          |> List.reduce ~f:max
          |> (fun x -> Option.value_exn x)
in
let rec process_streams map streams =
  match streams with
  | [] -> false, false
  | stream :: t ->
    (* let _ = print_map ground_map {x= stream.x; y=stream.y} 10 in *)
    let moved_to, tile_at_coord = move_stream_down map stream in
    let branch_flow, is_water = match moved_to with
      | Ok coord -> begin match coord with
        | c when c.y > max_y ->
          false, false
        | _ ->
          let _ = Hashtbl.set map coord Flow in
          let result = process_streams map (coord :: t) in
          (fst result), false
        end
      | Error coord ->
        match tile_at_coord with
        | None -> true, true
        | Some t -> match t with
          | Water -> true, false
          | Clay -> true, false
          | Flow -> false, false
    in
    if branch_flow then
      let left_moved_to, tile_left = move_stream_sideways ~dir:Left map stream in
      let left_allows_branch, is_water_left = match left_moved_to with
        | Ok coord ->
          let _ = Hashtbl.set map coord Water in
          let res = process_streams map (coord :: t) in
          res
        | Error coord ->
          true, true
      in
      let right_moved_to, tile_right = move_stream_sideways ~dir:Right map stream in
      let right_allows_branch, is_water_right = match right_moved_to with
        | Ok coord ->
          let _ = Hashtbl.set map coord Water in
          let res = process_streams map (coord :: t) in
          res
        | Error coord ->
          true, true
      in
      let all_water = is_water_left && is_water_right in
      let _ = if not all_water then
        make_layer_flow map stream
      else
        ()
      in
      (left_allows_branch && right_allows_branch), (is_water_right && is_water_left)
    else
      false, false
in
let streams = [ {x= 500 ; y= 0} ] in
let _ = process_streams ground_map streams in
let _ = Stdio.printf "New score %d\n" @@ calc_score ground_map in
let _ = map_to_pgm ground_map in
0

