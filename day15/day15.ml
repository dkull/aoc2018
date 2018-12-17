open Base
open Stdio
;;

type action = Move | Wait | Attack | NoEnemy ;;
type coordinates = {x: int ; y:int} ;;
type race = Elf | Goblin ;;
type unit = { race: race ; mutable hp: int ; power: int} ;;
type cell = Ground | Wall | Unit of unit;;
type loc_cell = { cell: cell ; loc: coordinates } ;;
type map = (cell Array.t) Array.t ;;

let parse_map_symbol token =
  match token with
  | '.' -> Ground
  | '#' -> Wall
  | 'E' -> Unit { race= Elf; hp= 200 ; power= 3 }
  | 'G' -> Unit { race= Goblin; hp= 200 ; power= 3}
  | _ -> raise @@ Failure "Bad map token"

let load_map =
  let rec read_line lineno map =
    let line = Stdio.In_channel.input_line Stdio.stdin in
    match line with
    | None -> Array.of_list_rev map
    | Some l ->
      let row_tokens = String.to_array l in
      let row = Array.map ~f:parse_map_symbol row_tokens in
      read_line (lineno+1) (row :: map)
  in
  read_line 0 []

let rec get_ordered_neighbors field cell =
  List.filter ~f:(fun coord ->
      coord.x >=0 && coord.x < (Array.length field.(0)) &&
      coord.y >=0 && coord.y < (Array.length field)
  ) [
    {x= cell.x; y= cell.y-1} ;
    {x= cell.x-1; y= cell.y} ;
    {x= cell.x+1; y= cell.y} ;
    {x= cell.x; y= cell.y+1} ;
  ]
;;

let get_matrix_value field cell =
  field.(cell.y).(cell.x)
;;

let integration_field map coords =
  let set_distance field cell distance =
    Array.set (field.(cell.y)) cell.x distance
  in
  let get_cell_cost map cell =
    match map.(cell.y).(cell.x) with
    | Ground -> 1
    | _ -> 255
  in
  let rec handle_neighbors map field neighbors current_distance =
    match neighbors with
    | [] -> []
    | neighbor :: t ->
      let cost = get_cell_cost map neighbor in
      let distance = current_distance + cost in
      let old_distance = get_matrix_value field neighbor in
      let dist_went_down = distance < old_distance in
      if dist_went_down then
        let _ = set_distance field neighbor distance in
        neighbor :: (handle_neighbors map field t current_distance)
      else
        let _ = Stdio.printf "Dropped %d,%d bc %d < %d\n" neighbor.x neighbor.y distance old_distance in
        handle_neighbors map field t current_distance
  in
  let rec walk queued_cells map field =
    match Linked_queue.dequeue queued_cells with
    | None ->
      let _ = Stdio.printf "No more free cells\n" in
      ()
    | Some c ->
      let _ = Stdio.printf "Handling cell %d,%d\n" c.x c.y in
      let current_distance = get_matrix_value field c in
      let neighbors = get_ordered_neighbors field c in
      let new_opens = handle_neighbors map field neighbors current_distance in
      let _ = List.iter new_opens ~f:(fun new_open ->
          let already_queued = Linked_queue.find queued_cells ~f:(fun z -> z.x = new_open.x && z.y = new_open.y) in
          match already_queued with
          | None ->
            let _ = Stdio.printf "Enquing something: %d,%d\n" new_open.x new_open.y in
            Linked_queue.enqueue queued_cells new_open
          | Some c ->
            let _ = Stdio.printf "Already enqueued: %d,%d\n" c.x c.y in
            ()
        )
      in
      let _ = Stdio.printf "Calling new walk with %d frees\n" (Linked_queue.length queued_cells) in
      walk queued_cells map field
  in
  let map_width, map_height = Array.length map.(0), Array.length map in
  let field = Array.make_matrix ~dimx:map_width ~dimy:map_height 255 in
  let _ = Array.set field.(coords.y) coords.x 0 in
  let open_cells = Linked_queue.of_list [{x= coords.x; y= coords.y}] in
  let _ = walk open_cells map field in
  field
;;

let print_integration field =
  Array.iter field ~f:(fun x ->
      let _ = Array.iter x ~f:(fun y -> Stdio.printf "%3d " y) in
      Stdio.printf "\n"
    )
;;

let print_map map =
  Array.iter map ~f:(fun x ->
      let _ = Array.iter x ~f:(fun y -> Stdio.printf "%c " (match y with
          | Ground -> '.'
          | Wall -> '#'
          | Unit u -> match u.race with | Elf -> 'E' | Goblin -> 'G'
        )) in
      Stdio.printf "\n"
    )
;;


let round_unit_order (map: cell Array.t Array.t) =
  let rec byrow rownr map =
    match map with
    | [] -> []
    | row :: t ->
      let units_in_row = Array.filter_mapi row ~f:(fun colnr data ->
        match data with
          | Unit u ->
            Some ({x= colnr; y= rownr}, u)
          | _ -> None
        )
      in
      units_in_row :: (byrow (rownr+1) t)
  in
  let res_rows = byrow 0 (Array.to_list map) in
  Array.to_list @@ Array.concat res_rows
;;

let get_attackable_enemy map coord unit =
  let neighbor_cells = get_ordered_neighbors map coord in
  let enemies = List.filter_map neighbor_cells ~f:(fun cell ->
    match get_matrix_value map cell with
    | Unit u -> if phys_equal u.race unit.race then
        None
      else
        Some (cell, u)
    | _ -> None
  )
  in
  let by_hp = List.stable_sort enemies ~compare:(fun a b -> compare (snd a).hp (snd b).hp) in
  List.hd by_hp
;;

let attack_enemy map power enemy =
  let _ = Stdio.printf "Attacking enemy\n" in
  let coord, unit  = enemy in
  let _ = unit.hp <- unit.hp - power in
  if unit.hp <= 0 then
    let _ = Stdio.printf "Unit %d,%d is dead\n" coord.x coord.y in
    Array.set map.(coord.y) coord.x Ground
  else
    ()
;;

let walk_or_stand map unit coords =
  let enemies = 0 in 0
;;

let do_action map coord unit =
  let attackable_enemy = get_attackable_enemy map coord unit in
  match attackable_enemy with
  | Some e -> attack_enemy map unit.power e
  | None -> walk_or_stand map unit coord
;;

let rec mainloop roundnr map =
  let unit_action (coord, unit) =
    if unit.hp > 0 then
      do_action map coord unit
    else
      ()
  in
  let unit_order = round_unit_order map in
  let _ = List.map unit_order ~f:unit_action in
  let _ = Stdio.printf "Ended round %d with %d units left\n" roundnr (List.length unit_order) in
  let _ = print_map map in
  mainloop (roundnr+1) map
;;


let map = load_map in
mainloop 0 map
