open Base
open Stdio

let data = In_channel.input_lines Stdio.stdin

type resistance = Weak | Immune

type dmg_type = Bludgeoning | Fire | Slashing | Cold | Radiation

type group =
  { immunes: bool
  ; id: int
  ; mutable units: int
  ; hp: int
  ; dmg_category: dmg_type
  ; mutable dmg: int
  ; initiative: int
  ; resistance: (dmg_type, resistance) Hashtbl.Poly.t }

let parse_resistance value =
  match value with "immune" -> Some Immune | "weak" -> Some Weak | _ -> None

let parse_dmg_category value =
  match value with
  | "fire" -> Some Fire
  | "bludgeoning" -> Some Bludgeoning
  | "cold" -> Some Cold
  | "radiation" -> Some Radiation
  | "slashing" -> Some Slashing
  | _ -> None

let clean_resistance token =
  if String.contains token '(' then
    let res = String.split token ~on:'(' in
    List.last_exn res
  else if String.contains token ')' then
    let res = String.split token ~on:')' in
    List.hd_exn res
  else if String.contains token ';' then
    let res = String.split token ~on:';' in
    List.hd_exn res
  else if String.contains token ',' then
    let res = String.split token ~on:',' in
    List.hd_exn res
  else token

let parse_resistances tokens =
  let map = Hashtbl.Poly.create () in
  let rec by_token index ?mode () =
    let token = tokens.(index) in
    let is_last = String.contains token ')' in
    let cleaned_token = clean_resistance token in
    match parse_resistance cleaned_token with
    | Some res -> by_token (index + 1) ~mode:res ()
    | None -> (
      match parse_dmg_category cleaned_token with
      | None -> by_token (index + 1) ~mode:(Option.value_exn mode) ()
      | Some cat ->
          Hashtbl.set map cat (Option.value_exn mode) ;
          if not is_last then
            by_token (index + 1) ~mode:(Option.value_exn mode) ()
          else () )
  in
  let () = by_token 7 () in
  map

let parse_group immunes id line =
  let is_header = String.contains line ':' in
  let is_empty = String.length line = 0 in
  match is_header || is_empty with
  | true -> None
  | false ->
      let tokens = Array.of_list @@ String.split ~on:' ' line in
      let length = Array.length tokens in
      let is_long = length > 18 in
      let resistances =
        if is_long then parse_resistances tokens else Hashtbl.Poly.create ()
      in
      Some
        { immunes
        ; id
        ; units= Int.of_string @@ tokens.(0)
        ; hp= Int.of_string @@ tokens.(4)
        ; dmg_category=
            Option.value_exn (parse_dmg_category tokens.(length - 5))
        ; dmg= Int.of_string @@ tokens.(length - 6)
        ; initiative= Int.of_string tokens.(length - 1)
        ; resistance= resistances }

let goods, bads =
  let rec by_line lines g b ~immunes =
    match lines with
    | [] -> (g, b)
    | line :: t -> (
        let id = if immunes then List.length g + 1 else List.length b + 1 in
        let new_group = parse_group immunes id line in
        let still_immunes = String.length line > 0 && immunes in
        match (immunes, new_group) with
        | _, None -> by_line t g b still_immunes
        | true, Some ng -> by_line t (ng :: g) b still_immunes
        | false, Some ng -> by_line t g (ng :: b) still_immunes )
  in
  by_line data [] [] ~immunes:true

let all_groups = List.append goods bads

let effective_power group = group.units * group.dmg

let tgt_select a b =
  let a_eff_p = effective_power a in
  let b_eff_p = effective_power b in
  if a_eff_p = b_eff_p then compare b.initiative a.initiative
  else compare b_eff_p a_eff_p

let calc_damage dealer target =
  let union = Hashtbl.find target.resistance dealer.dmg_category in
  let multiplier =
    match union with None -> 1 | Some Immune -> 0 | Some Weak -> 2
  in
  let eff_p = effective_power dealer in
  let result = eff_p * multiplier in
  result

let pair_up assigned_targets all_groups group =
  let free_targets =
    List.filter all_groups ~f:(fun other_group ->
        (not (Bool.compare other_group.immunes group.immunes = 0))
        && not (Hash_set.mem assigned_targets other_group) )
  in
  let correct_targets =
    List.sort free_targets ~compare:(fun a b ->
        let dmg_to_a = calc_damage group a in
        let dmg_to_b = calc_damage group b in
        if dmg_to_a = dmg_to_b then tgt_select a b
        else compare dmg_to_b dmg_to_a )
  in
  match correct_targets with
  | [] -> (group, None)
  | h :: t ->
      let dmg = calc_damage group h in
      if dmg > 0 then
        let () = Hash_set.add assigned_targets h in
        (group, Some h)
      else (group, None)

let attack dealer receiver =
  let dmg = calc_damage dealer receiver in
  let dead_units =
    Int.of_float @@ Float.round_down
    @@ (Float.of_int dmg /. Float.of_int receiver.hp)
  in
  let killable = min receiver.units dead_units in
  receiver.units <- receiver.units - killable ;
  killable

let both_teams_alive groups =
  List.fold groups ~init:(0, 0) ~f:(fun accum thing ->
      if thing.immunes then (fst accum + thing.units, snd accum)
      else (fst accum, snd accum + thing.units) )

let rec fight all_groups boosted () =
  let all_live =
    List.filter all_groups ~f:(fun group -> effective_power group > 0)
  in
  let alive_teams = both_teams_alive all_live in
  match alive_teams with
  | goods, bads when goods > 0 && bads > 0 ->
      let round_targets = Hash_set.Poly.create () in
      let first_order = List.sort all_live ~compare:tgt_select in
      let find_round_target = pair_up round_targets first_order in
      let paired_up = List.map first_order ~f:find_round_target in
      let attack_order =
        List.sort paired_up ~compare:(fun a b ->
            let atckr_a, _ = a in
            let atckr_b, _ = b in
            compare atckr_b.initiative atckr_a.initiative )
      in
      let something_died = ref false in
      let () =
        List.iter attack_order ~f:(fun (attacker, victim) ->
            match victim with
            | None -> ()
            | Some vic ->
                if effective_power attacker > 0 then
                  let dead_units = attack attacker vic in
                  let () =
                    if dead_units > 0 then something_died := true else ()
                  in
                  ()
                else () )
      in
      if !something_died then fight all_live boosted () else false
  | goods, bads when goods > 0 && bads = 0 ->
      printf "Part2:%d\n"
        (List.fold all_live ~init:0 ~f:(fun acc thng ->
             if thng.immunes && thng.units > 0 then acc + thng.units else acc
         )) ;
      true
  | _, _ -> (
    match boosted with
    | false ->
        let () =
          printf "Part1:%d\n"
            (List.fold all_live ~init:0 ~f:(fun acc thng -> acc + thng.units))
        in
        false
    | true -> false )

let boost_immunes groups boost =
  List.map groups ~f:(fun group ->
      if group.immunes then {group with dmg= group.dmg + boost}
      else {group with id= group.id} )

;;
let rec try_with_boost boost =
  let is_boosted = boost > 0 in
  let boosted_group = boost_immunes all_groups boost in
  let () = Out_channel.flush Stdio.stdout in
  let immune_win = fight boosted_group is_boosted () in
  match immune_win with
  | false -> try_with_boost (boost + 1)
  | true -> Caml.exit 0
in
try_with_boost 0
