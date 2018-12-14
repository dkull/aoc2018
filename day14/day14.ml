open Base
open Stdio
;;

let take_elf_recipe elf recipes =
  Hashtbl.find_exn recipes elf

let new_elf_pos elf recipes =
  let steps = (take_elf_recipe elf recipes) + 1 in
  (elf + steps) % (Hashtbl.length recipes)

let rec grow_recipes olds news =
  match news with
  | [] -> ()
  | h :: t ->
    let _ = Hashtbl.add olds (Hashtbl.length olds) h in
    grow_recipes olds t

let init_recipes target =
  let recipes = Hashtbl.create (module Int) in
  let _ = Hashtbl.add recipes 0 3 in
  let _ = Hashtbl.add recipes 1 7 in
  recipes

let create_recipes elves recipes =
  let rec over_elves elves recipes accum =
    match elves with
    | [] -> accum
    | elf :: t -> over_elves t recipes (accum + (take_elf_recipe elf recipes))
  in
  let sum = over_elves elves recipes 0 in
  if sum >= 10 then
    [1 ; sum % 10 ]
  else
    [sum]

let move_elves elves recipes =
  let rec over_elves elves recipes new_positions =
    match elves with
    | [] -> new_positions
    | elf :: t ->
      let new_elf = new_elf_pos elf recipes in
      over_elves t recipes (new_elf :: new_positions)
  in
  over_elves elves recipes []

let rec print_result recipes lastn =
  let count = Hashtbl.length recipes in
  let _ = Stdio.printf "%d" (Hashtbl.find_exn recipes (count+lastn-1)) in
  match lastn with
  | a when a = 0 -> ()
  | _ -> print_result recipes (lastn + 1)

let get_part2_target tgt =
  let str_repr = Int.to_string tgt in
  let tokens = String.to_list str_repr in
  let ints = List.map ~f:(fun x -> Int.of_string @@ Char.to_string x) tokens in
  ints

let comp_list a b =
  let result = List.fold_left ~init:true ~f:(fun acc x ->
      (fst x) = (snd x) && acc
    )
    (List.zip_exn a b)
  in
  result

let part2_match recipes target =
  let tgtcount = List.length target in
  let recipes_count = Hashtbl.length recipes in
  if tgtcount >= recipes_count then
      None
    else
      let rec getlast recipes offset =
        match offset with
        | 0 -> []
        | _ ->
          let recp = Hashtbl.find_exn recipes (recipes_count-offset) in
          recp :: getlast recipes (offset-1)
      in
      let last_recipes = getlast recipes (List.length target) in
      let are_match =  comp_list target last_recipes in
      if are_match then
        let _ = Stdio.printf "Returning MATCH\n" in
        Some (recipes_count - (List.length target))
      else
        None

let part1done = ref false ;;
let part2done = ref false ;;

let rec mainloop until recipes elves p2target =
  let _ = if !part1done || !part2done then
    raise @@ Failure "Done"
  else
    ()
  in
  let new_recipes = create_recipes elves recipes in
  let _ = grow_recipes recipes new_recipes in
  let new_elves = move_elves elves recipes in
  let part2result = part2_match recipes p2target in
  let _ = match part2result with
    | None -> ()
    | Some res ->
      let _ = Stdio.printf "Part2 answer: %d\n" res in
      let _ = part2done := true in
      ()
  in
  match (Hashtbl.length recipes) with
  | a when a = until+10 ->
    let _ = print_result recipes (-9) in
    let _ = part1done := true in
    mainloop until recipes new_elves p2target
  | _ ->
    mainloop until recipes new_elves p2target
;;

let target = 824501 in
let p2target = get_part2_target target in
let recipes = init_recipes target in
let elves = [0 ; 1] in
mainloop target recipes elves p2target
