open Base
open Stdio
open Caml.Stack
;;

(* Make xy hashable - really want to know a better way *)
type xy = { x: int ; y: int } ;;
module Coordinate = struct
  module T = struct
    type t = xy
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

type room = { t: bool ; b: bool ; l: bool ; r: bool } ;;
let default_room = { t= false ; b= false ; l= false ; r= false } ;;
type directions = N | S | E | W ;;

let field = Hashtbl.create (module Coordinate) ;;

let box_draw room =
  let tuple_room = room.t, room.b, room.l, room.r in
  match tuple_room with
  | true, true, true, true -> "╬"
  | false, true, true, true -> "╦"
  | true, false, true, true -> "╩"
  | true, true, true, false -> "╣"
  | true, true, false, true -> "╠"
  | false, false, true, true -> "═"
  | true, true, false, false -> "║"
  | false, false, false, true -> "["
  | false, false, true, false -> "]"
  | true, false, false, false -> "U"
  | false, true, false, false -> "A"
  | true, false, false, true -> "╚"
  | false, true, false, true -> "╔"
  | false, true, true, false -> "╗"
  | true, false, true, false -> "╝"
  | _ -> "?"
;;

let print_map ?at () =
  let coords = List.sort ~compare:Coordinate.compare @@ Hashtbl.keys field in
  let xs = List.map coords ~f:(fun xy -> xy.x ) in
  let ys = List.map coords ~f:(fun xy -> xy.y ) in

  let min_x = List.reduce_exn xs ~f:min in
  let max_x = List.reduce_exn xs ~f:max in
  let min_y = List.reduce_exn ys ~f:min in
  let max_y = List.reduce_exn ys ~f:max in

  let coord_map = Array.cartesian_product
      (Array.of_list @@ List.range min_y ~stop:`inclusive max_y)
      (Array.of_list @@ List.range min_x ~stop:`inclusive max_x)
  in
  let curr_y = ref 9999 in
  let _ = Array.iter coord_map ~f:(
    fun coord_pair ->
      let xy = {x=(snd coord_pair) ; y=(fst coord_pair)} in
      let _ = if not (!curr_y = xy.y) then
          let _ = Stdio.printf "\n%3.d - " xy.y in
          curr_y := xy.y
        else
          ()
      in
      let symbol =
        match at with
        | Some at when at.x = xy.x && at.y = xy.y -> "X"
        | Some _ | None ->
          match Hashtbl.find field xy with
          | None -> "."
          | Some room -> box_draw room
      in
      Stdio.printf "%s" symbol
  )
  in
  Stdio.printf "\n"
;;

let direction_of_token token =
  match token with
  | 'N' -> N
  | 'S' -> S
  | 'E' -> E
  | 'W' -> W
  | _ -> raise @@ Failure "Bad direction token"
;;

let inverse_direction direction =
  match direction with
  | N -> S
  | S -> N
  | E -> W
  | W -> E
;;

let target_location at_xy direction =
  match direction with
  | N -> { at_xy with y = at_xy.y - 1 }
  | S -> { at_xy with y = at_xy.y + 1 }
  | E -> { at_xy with x = at_xy.x + 1 }
  | W -> { at_xy with x = at_xy.x - 1 }
;;

let create_door at_xy direction =
  let source_room = Hashtbl.find_or_add field at_xy ~default:(fun () -> default_room) in
  match direction with
  | N -> Hashtbl.set field at_xy {source_room with t = true}
  | S -> Hashtbl.set field at_xy {source_room with b = true}
  | E -> Hashtbl.set field at_xy {source_room with r = true}
  | W -> Hashtbl.set field at_xy {source_room with l = true}
;;

let handle_token (at_xy:xy) token =
  let new_xy = match token with
  | '^' | '$' -> None
  | '(' | ')' | '|' -> None
  | 'N' | 'S' | 'E' | 'W' as direction_token ->
    let direction = direction_of_token direction_token in
    let to_xy = target_location at_xy direction in
    let _ = create_door at_xy direction in
    let _ = create_door to_xy (inverse_direction direction) in
    Some to_xy
  | _ -> raise @@ Failure "Unhandled token"
  in
  new_xy
;;

let rec mainloop at_xy tokens continues =
  match tokens with
  | [] -> []
  | token :: t ->
    let new_xy = handle_token at_xy token in
    match new_xy with
    | Some xy ->
      (* let _ = Stdio.printf "%c From %d,%d to %d,%d\n" token at_xy.x at_xy.y xy.x xy.y in *)
      (* let _ = print_map ~at:xy () in *)
      mainloop xy t continues
    | None ->
      (* let _ = Stdio.printf "%c\n" token in *)
      match token with
      | '(' ->
        let _ = Caml.Stack.push at_xy continues in
        mainloop at_xy t continues
      | '|' ->
        if Caml.Stack.is_empty continues then
          mainloop at_xy t continues
        else
          let prev_cont = Caml.Stack.top continues in
          mainloop prev_cont t continues
      | ')' ->
        let _ = Caml.Stack.pop continues in
        mainloop at_xy t continues
      | '^' | '$' -> mainloop at_xy t continues
      | _ -> raise @@ Failure "Badtoken"
;;

let input_line = Stdio.In_channel.input_line_exn Stdio.stdin  ;;
let initial_coordinate = { x= 0 ; y= 0 } ;;

let continue_stack = Caml.Stack.create () ;;
mainloop initial_coordinate (String.to_list input_line) continue_stack ;;

print_map () ;;

