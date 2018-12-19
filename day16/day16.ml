open Base
open Stdio
;;

let op_addr regs a b c = Array.set regs c @@
  (+) regs.(a) regs.(b)
let op_addi regs a b c = Array.set regs c @@
  (+) regs.(a) b
let op_mulr regs a b c = Array.set regs c @@
  regs.(a) * regs.(b)
let op_muli regs a b c = Array.set regs c @@
  regs.(a) * b
let op_banr regs a b c = Array.set regs c @@
  regs.(a) land regs.(b)
let op_bani regs a b c = Array.set regs c @@
  regs.(a) land b
let op_borr regs a b c = Array.set regs c @@
  regs.(a) lor regs.(b)
let op_bori regs a b c = Array.set regs c @@
  regs.(a) lor b
let op_setr regs a b c = Array.set regs c @@
  regs.(a)
let op_seti regs a b c = Array.set regs c @@
  a
let op_gtir regs a b c = Array.set regs c @@
  if a > regs.(b) then 1 else 0
let op_gtri regs a b c = Array.set regs c @@
  if regs.(a) > b then 1 else 0
let op_gtrr regs a b c = Array.set regs c @@
  if regs.(a) > regs.(b) then 1 else 0
let op_eqir regs a b c = Array.set regs c @@
  if a = regs.(b) then 1 else 0
let op_eqri regs a b c = Array.set regs c @@
  if regs.(a) = b then 1 else 0
let op_eqrr regs a b c = Array.set regs c @@
  if regs.(a) = regs.(b) then 1 else 0

let all_ops = [
  op_addr ;
  op_addi ;
  op_mulr ;
  op_muli ;
  op_banr ;
  op_bani ;
  op_borr ;
  op_bori ;
  op_setr ;
  op_seti ;
  op_gtir ;
  op_gtri ;
  op_gtrr ;
  op_eqir ;
  op_eqri ;
  op_eqrr ;
]

let input_lines = Stdio.In_channel.input_lines Stdio.stdin  ;;

let opcode_eliminator =
  let map = Hashtbl.create (module Int) ~size:16 in
  let _ = List.iteri all_ops (fun i _ ->
      let feature_map = Array.create ~len:16 true in
      Hashtbl.set map ~key:i ~data:feature_map
    )
  in
  map
;;

let count_op_matches bef ins aft =
  let rec over_ops iteration op_funs =
    match op_funs with
    | [] -> 0
    | op_fun :: t ->
      let regs = Array.copy bef in
      let op, a, b, c = ins.(0), ins.(1), ins.(2), ins.(3) in
      let _ = op_fun regs a b c in
      let paired = Array.zip_exn regs aft in
      let test_match = Array.fold ~init:true paired ~f:(fun a b ->
          a && phys_equal (fst b) (snd b)
        )
      in
      let matchcount = if test_match then
        1
      else
        let opcode_features = Hashtbl.find_exn opcode_eliminator op in
        let _ = Array.set opcode_features iteration false in
        0
      in
      matchcount + (over_ops (iteration+1) t)
  in
  over_ops 0 all_ops
;;

let extract_data line =
  let catch_numbers1 = Str.regexp ".*\\[\([0-9]+\), \([0-9]+\), \([0-9]+\), \([0-9]+\)\\].*" in
  let catch_numbers2 = Str.regexp "^\([0-9]+\) \([0-9]+\) \([0-9]+\) \([0-9]+\).*" in
  let first_worked = Str.string_match catch_numbers1 line 0 in
  let _ = if first_worked then
      ()
    else
      let _ = Str.string_match catch_numbers2 line 0 in
      ()
  in
  Array.of_list [Int.of_string @@ Str.matched_group 1 line;
   Int.of_string @@ Str.matched_group 2 line;
   Int.of_string @@ Str.matched_group 3 line;
   Int.of_string @@ Str.matched_group 4 line]
;;

let test_input_aligned first_line =
  let not_empty = not ((String.length first_line) = 0) in
  not_empty && phys_equal (String.get first_line 0) 'B'
;;

let rec parse_and_run_tests lines part1samples =
  match lines with
  | bef :: ins :: aft :: t ->
    let aligned = test_input_aligned bef in
    if aligned then
      let _ = Stdio.printf "-> %s @ %s -> %s\n" bef ins aft in
      let bef_data = extract_data bef in
      let ins_data = extract_data ins in
      let aft_data = extract_data aft in
      let matched_op_count = count_op_matches bef_data ins_data aft_data in
      let is_part1_case = matched_op_count >= 3 in
      if is_part1_case then
        parse_and_run_tests t (part1samples+1)
      else
        parse_and_run_tests t part1samples
    else
      parse_and_run_tests (ins :: aft :: t) part1samples
  | _ -> part1samples
in
let part1result = parse_and_run_tests input_lines 0 in
Stdio.printf "Part1: %d\n" part1result

(* part 2 *)

let rec calculate_mapping mapping () =
  let _ = Hashtbl.filter_mapi_inplace opcode_eliminator ~f:(fun ~key:k ~data:d ->
      let guess_count = Array.count d ~f:(fun flag -> flag) in
      let guess_index, _ = Array.findi_exn d ~f:(fun i flag -> flag) in
      if guess_count = 1 then
        let _ = Stdio.printf "%d is %d'th\n" k guess_index in
        let _ = Array.set mapping k guess_index in
        let _ = Hashtbl.iter opcode_eliminator ~f:(fun data ->
            Array.set data guess_index false
          ) in
        None
      else
        Some d
    )
  in
  let unresolved = Array.count mapping ~f:(fun v -> v = (-1)) in
  match unresolved with
  | 0 -> ()
  | _ -> calculate_mapping mapping ()
;;

let intermediate_mapping = Array.create ~len:16 (-1) in
let _ = calculate_mapping intermediate_mapping () in
let final_mapping =
  Array.map intermediate_mapping ~f:(fun v ->
      List.nth_exn all_ops v
  )
in
let run_instruction inst reg =
  let op, a, b, c = inst.(0), inst.(1), inst.(2), inst.(3) in
  let instruction = final_mapping.(op) in
  instruction reg a b c
in
let program_result =
  let regs = Array.create ~len:4 0 in
  let rec run_program lines seq_newlines=
    match lines with
    | [] -> ()
    | h :: t ->
      if seq_newlines = 3 then
        let inst = extract_data h in
        let _ = run_instruction inst regs in
        run_program t seq_newlines
      else
        let line_length = String.length h in
        if line_length = 0 then
          run_program t (seq_newlines+1)
        else
          run_program t 0
  in
  let _ = run_program input_lines 0 in
  regs
in
Stdio.printf "Reg A: %d\n" program_result.(0)


