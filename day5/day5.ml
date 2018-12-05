let can_destroy a b =
  Char.code a - Char.code b |> abs = 32
in
let data_to_list data =
  List.of_seq @@ String.to_seq data
in
let process_output output =
  List.to_seq output |> String.of_seq
in
let rec strip_units data units =
  match units with
  | [] -> data
  | h :: t ->
    let stripped_data = List.filter (fun x -> x != h) data in
    strip_units stripped_data t
in
let rec process_polymers (past_data: char list) (future_data: char list) =
  match future_data with
  | [] -> List.rev past_data
  | h :: [] ->
    process_polymers (h :: past_data) []
  | h :: hh :: t ->
    let destroy = can_destroy h hh in
    if destroy then
      (* backtrack by one item and rerun *)
      match past_data with
      (* if no past data then just proceed *)
      | [] -> process_polymers [] t
      | past_h :: past_t -> process_polymers past_t (past_h :: t)
    else
      process_polymers (h :: past_data) (hh :: t)
in
let main =
  (* we have to read a single line only *)
  let data = data_to_list @@ read_line () in
  let output = process_polymers [] data in
  let processed_output = process_output output in
  (* part 2 *)
  let without_aa = process_polymers [] (strip_units output ['A';'a']) in
  let without_bb = process_polymers [] (strip_units output ['B';'b']) in
  let without_cc = process_polymers [] (strip_units output ['C';'c']) in
  let without_dd = process_polymers [] (strip_units output ['D';'d']) in
  (* output *)
  Printf.printf "%s\n" processed_output;
  Printf.printf "Which has %d digits\n" @@ String.length processed_output;
  Printf.printf "Aa %d Bb %d Cc %d Dd %d\n"
    (List.length without_aa) (List.length without_bb) (List.length without_cc) (List.length without_dd);
in
main
