open Base
open Stdio
;;

type heading = Left | Right | Up | Down ;;
type move = Left | Right | Straight ;;
type cart = { mutable x: int ; mutable y: int ; mutable nextmove: move ; mutable heading: heading ; mutable crashed: bool} ;;

(* mostly for printing *)
let repr_heading heading =
  match heading with
  | Up -> '^'
  | Down -> 'v'
  | Left -> '<'
  | Right -> '>'
;;

let map_tile cart map =
  map.(cart.y).(cart.x)
;;

let find_carts line lineno =
  let rec bychar chars index =
    match chars with
    | [] -> []
    | h :: t
      when phys_equal h '<'
        || phys_equal h '>'
        || phys_equal h '^'
        || phys_equal h 'v' ->
      let heading = match h with
        | '^' -> Up
        | 'v' -> Down
        | '<' -> Left
        | '>' -> Right
        | _ -> raise @@ Failure "bad"
      in
      { crashed= false; x= index ;y= lineno; nextmove= Left; heading= heading} :: (bychar t (index+1))
    | _ :: t -> bychar t (index+1)
  in
  bychar (String.to_list line) 0
;;

let load_track =
  let rec byline lineno map carts =
    let line = Stdio.In_channel.input_line Stdio.stdin in
    match line with
    | None -> Array.of_list_rev(map), carts
    | Some l ->
      let new_carts = find_carts l lineno in
      byline (lineno+1) (String.to_array l :: map) (List.append carts new_carts)
  in
  byline 0 [] []

let print_map map carts =
  let _ = Array.mapi map ~f:(fun i x ->
      let _ = Array.mapi x ~f:(fun j y ->
          let on_cart = List.find carts ~f:(fun c -> c.x = j && c.y = i) in
          match on_cart with
          | None -> Stdio.printf "%c" y
          | Some c -> Stdio.printf "%c" (repr_heading c.heading)
        ) in
      Stdio.printf "\n"
    )
  in
  ()
;;

let clean_map map carts =
  let rec bycart carts =
    match carts with
    | [] -> ()
    | cart :: t ->
      let row = map.(cart.y) in
      let _ = Array.replace row (cart.x) ~f:(fun _ ->
        match cart.heading with
        | Right | Left -> '-'
        | Up | Down -> '|')
      in
      bycart t
  in
  bycart carts
;;

let apply_heading cart =
  match cart.heading with
  | Up -> cart.y <- (cart.y -1)
  | Down -> cart.y <- (cart.y +1)
  | Left -> cart.x <- (cart.x -1)
  | Right -> cart.x <- (cart.x +1)
;;

let crossroads cart =
  let next = cart.nextmove in
  let heading = match (cart.heading, next) with
  | (_, Straight) as h -> (fst h)
  | (Up, Left) -> Left
  | (Up, Right) -> Right
  | (Down, Left) -> Right
  | (Down, Right) -> Left
  | (Left, Left) -> Down
  | (Left, Right) -> Up
  | (Right, Left) -> Up
  | (Right, Right) -> Down
  in

  let _ = cart.nextmove <- (match cart.nextmove with
      | Left -> Straight
      | Straight -> Right
      | Right -> Left
    )
  in
  heading
;;

let turn cart turnmark =
  match cart.heading, turnmark with
  | (Left, '/') -> Down
  | (Right, '/') -> Up
  | (Up, '/') -> Right
  | (Down, '/') -> Left
  | (Left, '\\') -> Up
  | (Right, '\\') -> Down
  | (Up, '\\') -> Left
  | (Down, '\\') -> Right
  | _ -> raise @@ Failure "Bad turning"
;;

let move_cart carts cart map =
  let _ = apply_heading cart in
  let standing_on = map_tile cart map in
  let crashed_with = List.find carts ~f:(fun oc ->
      oc.x = cart.x && oc.y = cart.y && not (phys_equal oc cart) && (not oc.crashed) && (not cart.crashed)) in
  let _ = if Option.is_some crashed_with then
    ()
  in
  match crashed_with with
  | None -> 
    let newheading = match standing_on with
    | '+' -> crossroads cart
    | '/' | '\\' as symbol -> turn cart symbol
    | '|' | '-' -> cart.heading
    | _ -> raise @@ Failure "Out of bounds"
    in
    let _ = cart.heading <- newheading in
    ()
  | Some other ->
    let _ = other.crashed <- true in
    let _ = cart.crashed <- true in
    ()
;;

let timetick carts cart map =
  match cart.crashed with
  | false ->
    let _ = move_cart carts cart map in
    not cart.crashed
  | true -> false
;;

let sort_cart a b =
  let y = compare a.y b.y in
  let x = compare a.x b.x in
  if not (y = 0) then
    y
  else
    x

let rec simulate map carts index =
  let carts = List.filter carts ~f:(fun x -> not x.crashed) in
  let carts = List.sort carts ~compare:sort_cart in
  let cartcount = List.length carts in
  let _ = Stdio.printf "\nSim nr %d with %d carts\n" index cartcount in
  let _ =
    if cartcount <= 1 then
      let lastcart = List.nth_exn carts 0 in
      let msg = Printf.sprintf "Yes, last cart @ %d %d!" lastcart.x lastcart.y in
      raise @@ Failure msg
    else
      ()
  in

  let carts = List.filter carts ~f:(fun x-> timetick carts x map) in
  simulate map carts (index+1)
;;

let (map, carts) = load_track;;

let _ = clean_map map carts ;;
let _ = print_map map carts ;;
simulate map carts 0

