open Base
open Stdio
;;

type enemy_search = MoveTo | Attack ;;
type action = Move | Wait | Attack | NoEnemy ;;
type coordinates = {x: int ; y:int} ;;
type race = Elf | Goblin ;;
type unit = { race: race ; mutable hp: int ; power: int ; mutable coord: coordinates} ;;
type cell = Ground | Wall | Unit of unit;;
type loc_cell = { cell: cell ; loc: coordinates } ;;
type map = (cell Array.t) Array.t ;;

let parse_map_symbol token coordinate elfpower =
  match token with
  | '.' -> Ground
  | '#' -> Wall
  | 'E' -> Unit { race= Elf; hp= 200 ; power= elfpower ; coord= coordinate}
  | 'G' -> Unit { race= Goblin; hp= 200 ; power= 3 ; coord= coordinate}
  | _ -> raise @@ Failure "Bad map token"

let input_lines = Stdio.In_channel.input_lines Stdio.stdin  ;;

let load_map lines ?(elfpower=3) =
  let rec parse_line lines lineno map =
    match lines with
    | [] -> Array.of_list_rev map
    | l :: t ->
      let row_tokens = String.to_array l in
      let row = Array.mapi ~f:(fun i d ->
          parse_map_symbol d {x= i; y=lineno} elfpower
        ) row_tokens in
      parse_line t (lineno+1) (row :: map)
  in
  parse_line lines 0 []
;;

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
    match (get_matrix_value map cell) with
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
        handle_neighbors map field t current_distance
  in
  let rec walk queued_cells map field =
    match Linked_queue.dequeue queued_cells with
    | None ->
      ()
    | Some c ->
      let current_distance = get_matrix_value field c in
      let neighbors = get_ordered_neighbors field c in
      let new_opens = handle_neighbors map field neighbors current_distance in
      let _ = List.iter new_opens ~f:(fun new_open ->
          let already_queued = Linked_queue.find queued_cells ~f:(fun z -> z.x = new_open.x && z.y = new_open.y) in
          match already_queued with
          | None ->
            Linked_queue.enqueue queued_cells new_open
          | Some c ->
            ()
        )
      in
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
  let rec byrow map =
    match map with
    | [] -> []
    | row :: t ->
      let units_in_row = Array.filter_map row ~f:(fun data ->
        match data with
          | Unit u -> Some u
          | _ -> None
        )
      in
      units_in_row :: (byrow t)
  in
  let res_rows = byrow (Array.to_list map) in
  Array.to_list @@ Array.concat res_rows
;;

let print_unit_healths map =
  let units = round_unit_order map in
  let unit_hps = List.map units ~f:(fun unit ->
      unit.hp
    ) in
  let sum_of_hp = List.reduce_exn unit_hps ~f:(+) in
  Stdio.printf "Unit hp total: %d\n" sum_of_hp

;;

let get_enemies ~attackable map unit =
  let all_units = round_unit_order map in
  let all_enemies = List.filter all_units ~f:(fun x -> not (phys_equal x.race unit.race)) in
  if attackable then
    let close_enemies = List.filter all_enemies ~f:(fun x ->
        1 = (abs (x.coord.x - unit.coord.x)) + (abs (x.coord.y - unit.coord.y))
      ) in
    let by_hp = List.stable_sort close_enemies ~compare:(fun a b -> compare a.hp b.hp) in
    by_hp
  else
    let far_enemies = List.filter all_enemies ~f:(fun x ->
        (abs (x.coord.x - unit.coord.x)) + (abs (x.coord.y - unit.coord.y))
        > 1
      ) in
    far_enemies
;;

let attack_enemy map power enemy =
  let _ = enemy.hp <- enemy.hp - power in
  if enemy.hp <= 0 then
    let _ = Stdio.printf "Unit at %d,%d died\n" enemy.coord.x enemy.coord.y in
    Array.set map.(enemy.coord.y) enemy.coord.x Ground
  else
    ()
;;

let find_move_to fields unit cheapest =
  let unit_at = unit.coord in
  let example_field = List.hd_exn fields in
  let neighboring_cells = get_ordered_neighbors example_field unit_at in
  let distances_for_neighbors = List.map neighboring_cells ~f:(fun nc ->
      let neighbor_values = List.map fields ~f:(fun field ->
          get_matrix_value field nc
        )
      in
      let best_value = List.reduce_exn neighbor_values ~f:min in
      best_value
    )
  in
  let paired = List.zip_exn neighboring_cells distances_for_neighbors in
  let best_spot = List.reduce_exn (List.rev paired) ~f:(fun a b ->
      if (snd b) <= (snd a) then
        b
      else
        a
    )
  in
  best_spot
;;

let move_unit map unit new_coord =
  let _ = Array.set (map.(unit.coord.y)) unit.coord.x Ground in
  let _ = Array.set (map.(new_coord.y)) new_coord.x (Unit unit) in
  let _ = unit.coord <- new_coord in
  ()
;;

let walk_or_stand map unit =
  let enemies = get_enemies ~attackable:false map unit in
  let integration_fields = List.map enemies ~f:(fun x ->
      integration_field map x.coord
    )
  in
  let closest_spots = List.map integration_fields ~f:(fun x ->
      let spots = get_ordered_neighbors x unit.coord in
      let values = List.map spots ~f:(fun sp -> get_matrix_value x sp) in
      values
    )
  in
  let cheapest = List.fold_left ~init:999 ~f:min (List.concat closest_spots) in
  match cheapest with
  | 255 -> ()
  | 999 -> Stdio.printf "NO ENEMIES LEFT\n"
  | _ ->
    let new_coord, distance = find_move_to integration_fields unit cheapest in
    let _ = move_unit map unit new_coord in
    ()
;;

let do_action map unit =
  let attackable_enemy = List.hd @@ get_enemies ~attackable: true map unit in
  match attackable_enemy with
  | Some e -> attack_enemy map unit.power e
  | None ->
    let _ = walk_or_stand map unit in
    let attackable_enemy = List.hd @@ get_enemies ~attackable: true map unit in
    match attackable_enemy with
    | Some e -> attack_enemy map unit.power e
    | None -> ()
;;

let get_live_units units =
   List.filter units ~f:(fun unit -> unit.hp > 0)

let check_done live_units (elf_count, _) =
  elf_count = 0 || elf_count = (List.length live_units)

let elf_stats live_units =
  let elves = List.filter live_units ~f:(fun unit -> phys_equal unit.race Elf) in
  let elf_count = List.length elves in
  let hp_sum = List.fold_left elves ~init:0 ~f:(fun a b -> a + b.hp) in
  elf_count, hp_sum

let rec mainloop roundnr map =
  let unit_action unit =
    if unit.hp > 0 then
      do_action map unit
    else
      ()
  in
  let _ = Stdio.printf "Starting round %d\n" roundnr in
  let unit_order = round_unit_order map in
  let _ = List.map unit_order ~f:unit_action in
  let live_units = get_live_units unit_order in
  let elf_stat = elf_stats live_units in
  let are_done = check_done live_units elf_stat in
  match are_done with
  | false ->
    (* let _ = print_map map in
    * let _ = print_unit_healths map in *)
    mainloop (roundnr+1) map
  | true -> elf_stat
;;

(* Run this for part1 *)

let map = load_map input_lines ~elfpower:3 in
mainloop 1 map

(* Run this for part2 *)

let rec boost_elves elf_power =
  let map = load_map input_lines ~elfpower:elf_power in

  let unit_order = round_unit_order map in
  let live_units = get_live_units unit_order in
  let initial_elves, _ = elf_stats live_units in

  let live_elves, hp_sum = mainloop 1 map in
  if live_elves = initial_elves then
    let _ = Stdio.printf "Initial %d after %d at power %d with hp %d\n" initial_elves live_elves elf_power hp_sum in
    Caml.exit 0
  else
    boost_elves (elf_power + 1)
;;
boost_elves 4

