exception BadLine of string ;;
exception NoDataInDiary of string ;;

type log_type = Begin | Sleep | Wake ;;

let read_input =
  let rec read_input got =
    try
      let line = read_line () in
      read_input @@ line :: got
    with End_of_file -> List.rev got
  in
  read_input []
in
let sort_input lines =
  List.sort String.compare lines
in
let cleanup_data lines =
  let cleanup_re = Str.regexp "\\[\\|\\]\\|#" in
  List.map (fun x -> Str.global_replace cleanup_re "" x) lines
in
let tokenise_lines lines =
  List.map (fun x -> String.split_on_char ' ' x) lines
in
let parse_type tokens =
  match List.nth tokens 2 with
  | "Guard" -> Begin
  | "falls" -> Sleep
  | "wakes" -> Wake
  | _ as t -> raise @@ BadLine ("Bad token: " ^ t)
in
let guard_from_tokens tokens =
  List.nth tokens 3
in
let parse_time time =
  let hour_min= String.split_on_char ':' time in
  List.nth hour_min 0 |> int_of_string, List.nth hour_min 1 |> int_of_string
in
let rec int_range_to_list from to_excl =
  if from = to_excl then
    []
  else
    from :: (int_range_to_list (from + 1) to_excl)
in
let rec record_stats diary_totals diary_minutes (guard:string) session_minutes =
  match session_minutes with
  | [] -> ()
  | h :: t ->
    let current_guard_total =
      match Hashtbl.find_opt diary_totals guard with
      | None -> 0
      | Some v -> v
    in
    let current_guard_minutes =
      match Hashtbl.find_opt diary_minutes (guard, h) with
      | None -> 0
      | Some v -> v
    in
    let _ = Hashtbl.replace diary_totals guard (current_guard_total+1) in
    let _ = Hashtbl.replace diary_minutes (guard, h) (current_guard_minutes+1) in
    record_stats diary_totals diary_minutes guard t
in
let get_most_slept_guard diary_totals =
  let items = List.of_seq @@ Hashtbl.to_seq diary_totals in
  let sort_by_total a b = compare (a |> snd ) (b |> snd) in
  let sorted_items = List.rev @@ List.sort sort_by_total items in
  List.nth sorted_items 0
in
let get_sleepiest_minute diary_minutes ~guard () =
  let items = List.of_seq @@ Hashtbl.to_seq diary_minutes in
  let items =
    (* if guard label is specified then filter out only the counts that are from that guard*)
    (* this optional arg is the only change I had to make for Part 2 *)
    match guard with
    | None -> items
    | Some guard -> List.filter (fun x -> (fst(fst(x))) = guard) items
  in
  let sort_by_total a b = compare (a |> snd ) (b |> snd) in
  let sorted_items = List.rev @@ List.sort sort_by_total items in
  List.nth sorted_items 0
in
let mark_down_sleeps diary_totals diary_minutes tokens =
  let rec iter_and_mark tokens guard prev_log_time prev_log_type =
    match tokens with
    | [] -> ()
    | h :: t ->
      let log_time = parse_time (List.nth h 1) in
      let log_type = parse_type h in
      match (prev_log_type, log_type) with
      (* provide new guard from Begin message *)
      | (Wake, Begin) -> iter_and_mark t (guard_from_tokens h) log_time log_type
      | (Sleep, Wake) ->
        (* need to extract the sleep minutes from this *)
        let from_minutes = snd prev_log_time in
        let to_minutes = snd log_time in
        let slept_minutes = int_range_to_list from_minutes to_minutes in
        let _ = record_stats diary_totals diary_minutes guard slept_minutes in
        iter_and_mark t guard log_time log_type
      | _ -> iter_and_mark t guard log_time log_type
  in
  (* code assumes Wake is before Begin, and data starts with Begin *)
  iter_and_mark tokens "" (-24,-59) Wake
in
let main =
  let lines = read_input in
  let lines = sort_input lines in
  let lines = cleanup_data lines in
  let tokens_of_line = tokenise_lines lines in
  let diary_totals = Hashtbl.create 2000 in
  let diary_minutes = Hashtbl.create 2000 in
  (* extract data and count sleep minutes into diaries *)
  let _ = mark_down_sleeps diary_totals diary_minutes tokens_of_line in
  let most_slept_guard = get_most_slept_guard diary_totals in
  (*let _ = Printf.printf "%s slept %d minutes\n" (fst most_slept_guard) (snd most_slept_guard) in*)
  let sleepiest_minute = get_sleepiest_minute diary_minutes (Some (fst @@ most_slept_guard)) () in
  let (_:unit) = Printf.printf "The guard %s slept most on minute %d -> %d times\n" (fst @@ fst sleepiest_minute) (snd @@ fst sleepiest_minute) (snd sleepiest_minute) in
  let part2_guard = get_sleepiest_minute diary_minutes None () in
  let (_:unit) = Printf.printf "The Part2 guard %s slept most on minute %d -> %d times\n" (fst @@ fst part2_guard) (snd @@ fst part2_guard) (snd part2_guard) in

  sleepiest_minute
in
main
