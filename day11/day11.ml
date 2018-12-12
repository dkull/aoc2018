open Base
open Stdio
;;

type coord = { x: int ; y: int } ;;
module PointInfo = struct
  module T = struct
    type t = coord
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

let snum = 2187 ;;

let calc_power_level x y =
  let rack_id = x + 10 in
  ((rack_id * y) + snum) * rack_id
  |> fun x -> (x / 100 % 10) - 5
;;

let power_levels =
  let lookup = Hashtbl.create (module PointInfo) ~size:(300*300) in
  let rec calcloop xat xfrom xto yat yfrom yto =
    let _ = Hashtbl.add lookup {x=xat; y=yat} (calc_power_level xat yat) in
    match (xat, yat) with
    | (x, y) when x = xto && y = yto -> ()
    | (x, _) when x = xto ->
      calcloop xfrom xfrom xto (yat + 1) yfrom yto
    | _ ->
      calcloop (xat+1) xfrom xto yat yfrom yto
  in
  let _ = calcloop 1 1 300 1 1 300 in
  lookup
;;


let generate_points x y size =
  let rec generator xat yat collected =
    let point = {x=x+xat; y=y+yat} in
    let new_collected = point :: collected in
    match (xat, yat) with
    | (x, y) when x = 0 && y = size -> collected
    | (x, y) when x = size-1 -> generator 0 (y+1) new_collected
    | (x, y) -> generator (x+1) y new_collected
  in
  generator 0 0 []
;;

let area_score lookup x y size =
  match (x, y) with
  | (a,b) when (a + size) > 300 || (b + size) > 300 -> None
  | _ -> 
    let points = generate_points x y size in
    let powers = List.map points ~f:(fun z -> Hashtbl.find lookup z) in
    let unusables = List.count powers ~f:(fun x -> Option.is_none x) in
    if unusables = 0 then
      let unwrapped = List.map powers ~f:(fun x -> Option.value_exn x) in
      let sum = List.reduce unwrapped ~f:(+) in
      match sum with
      | None -> None
      | Some x -> sum
    else
      None
;;

let prnt_hs x y s sc =
  Stdio.printf "Best new score: %d,%d size %d = %d \n" x y s sc
;;

let best_area pow_lvls =
  let best_size = ref 0 in
  let best_point = ref (0,0) in
  let highscore = ref 0 in
  let count = ref 0 in
  let rec sizeloop size =
    let _ = print_endline "--" in
    let _ = Stdio.printf "Doing size %d\n" size in
    let _ = Hashtbl.iteri pow_lvls ~f:(fun ~key:key ~data:value ->
        let _ = count := (!count + 1) in
        let score = area_score pow_lvls key.x key.y size in
        match score with
        | Some sc when sc > !highscore ->
          let _ = prnt_hs key.x key.y size sc in
          let _ = best_size := size in
          let _ = best_point := (key.x, key.y) in
          highscore := sc
        | None -> ()
        | Some _ -> ()
      )
    in
    match size with
    | 300 -> ()
    | _ -> sizeloop (size+1)
  in
  sizeloop 1
in
best_area power_levels
