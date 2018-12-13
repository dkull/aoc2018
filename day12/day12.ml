open Base
open Stdio
;;

type pot_state = Growing | Empty ;;

let pots_of_string str =
  let str_chars = List.rev @@ String.to_list_rev str in
  let rec translator inp =
    match inp with
    | [] -> []
    | h :: t ->
      let outp = match h with
      | '#' -> Growing
      | '.' -> Empty
      | _ -> raise @@ Invalid_argument "Not # nor ."
      in
      outp :: translator t
  in
  translator str_chars

let initial_state =
  Stdio.In_channel.input_line Stdio.stdin
  |> fun x -> Option.value_exn x
  |> String.split ~on:':'
  |> fun x -> List.nth_exn x 1
  |> String.strip
  |> pots_of_string

(* drop empty line *)
let _ = Stdio.In_channel.input_line Stdio.stdin

let rec gen_rules ()=
  let line = Stdio.In_channel.input_line Stdio.stdin in
  match line with
  | None -> []
  | Some s ->
    let tokens = String.split ~on:' ' s in
    let rule = pots_of_string @@ List.nth_exn tokens 0 in
    let result = List.hd_exn @@ pots_of_string (List.nth_exn tokens 2) in
    (rule, result) :: gen_rules ()

let grow_if_need state =
  let grew_left, new_state = match state with
  | Empty :: Empty :: Empty :: t -> 0, state
  | _ -> 3, (Empty :: Empty :: Empty :: state)
  in
  let grew_right, new_state = match (List.rev new_state) with
  | Empty :: Empty :: Empty:: t -> 0, new_state
  | _ as reved -> 3, (List.rev (Empty :: Empty :: Empty :: reved))
  in
  new_state, grew_left, grew_right
;;

let rec calc_part1 state offset accum =
  match state with
  | [] -> accum
  | h :: t ->
    match h with
    | Growing ->
      (* let _ = Stdio.printf "Adding %d to %d = %d\n" offset accum (offset+accum)in *)
      calc_part1 t (offset+1) (accum+offset)
    | Empty -> calc_part1 t (offset+1) (accum)
;;

let print_pots state iteration left_offset score =
  let _ = Stdio.printf "%d (->%d|%d)" (iteration) left_offset score in
  let _ = List.iter state ~f:(fun x ->
      Stdio.printf "%c" (match x with
      | Empty -> '.'
      | Growing -> '#'
    ))
  in
  let _ = Stdio.printf "\n" in
  score
;;

let rec match_rule (section: pot_state list) (rules: (pot_state list * pot_state) list) =
  match rules with
  | [] -> Empty
  | h :: t ->
    let rule, result = fst h, snd h in
    let combined = List.zip_exn rule section in
    let are_match = List.fold_left ~init:true combined
        ~f:(fun a d -> phys_equal (fst d) (snd d) && a) in
    if are_match then
      result
    else
      match_rule section t

let next_gen state (rules: (pot_state list * pot_state) list) =
  let grown_state, grown_left, grown_right = grow_if_need state in
  let rec do_chunk partialstate =
    match partialstate with
    | ll :: l :: c :: r :: rr :: t ->
      let changed_to = match_rule (ll :: l :: c :: r :: rr :: []) rules in
      changed_to :: do_chunk (l :: c :: r :: rr :: t)
    | _ -> []
  in
  let transformed = do_chunk grown_state in
  Empty :: Empty :: transformed, grown_left, grown_right
;;

let get_stable st newst =
  let a = List.drop_while st (fun x -> phys_equal x Empty) in
  let b = List.drop_while st (fun x -> phys_equal x Empty) in
  List.fold_left ~init:true ~f:(fun a d -> phys_equal (fst d) (snd d) && a) (List.zip_exn a b)
;;

let rules = gen_rules () in
let rec run_generation state iteration until left_offset prev_score prev_score_delta =
  let score = calc_part1 state left_offset 0 in
  let score_delta = score - prev_score in
  let _ = Stdio.printf "If stable then final score %d\n" (((until - iteration) * score_delta) + score) in
  let _ = print_pots state iteration left_offset score in
  let new_state, grew_left, grew_right  = next_gen state rules in
  match iteration with
  | i when i = until -> state, left_offset
  |_ ->
    run_generation new_state (iteration + 1) until (left_offset - grew_left) score score_delta
in
let state,offset = run_generation initial_state 0 50000000000 0 0 0 in
let part1result = calc_part1 state offset 0 in
Stdio.printf "Result is %d\n" part1result







