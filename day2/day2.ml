let day2 =
  let load_input =
    let rec read_input got =
      try
        let line = read_line () in
        read_input @@ line :: got
      with End_of_file -> List.rev got
    in
    let input = List.sort String.compare (read_input []) in
    let charlists = List.map (fun x -> x |> String.to_seq |> List.of_seq) input in
    charlists
  in
  let seq_has_val haystack needle =
    let matches = Seq.filter (fun x -> x = needle) haystack in
    let matchcount = matches |> List.of_seq |> List.length in
    match matchcount with
    | _ when matchcount > 0 -> true
    | _ -> false
  in
  let rec count_chars hashes line =
    match line with
    | [] -> ()
    | h :: t ->
      let oldcount = if Hashtbl.mem hashes h then Hashtbl.find hashes h else 0 in
      let _ = Hashtbl.replace hashes h (oldcount + 1) in
      count_chars hashes t
  in
  let rec parseline lines accum2 accum3 =
    match lines with
    | [] -> accum2 * accum3
    | h :: t ->
      let line = h in
      let hashes = Hashtbl.create 100 in
      let _ = count_chars hashes line in
      let vals = Hashtbl.to_seq_values hashes in
      let new_accum2 = if seq_has_val vals 2 then accum2 + 1 else accum2 in
      let new_accum3 = if seq_has_val vals 3 then accum3 + 1 else accum3 in
      parseline t new_accum2 new_accum3
  in
  (* Sort the strings for Part 2 *)
  let data = load_input in
  (* Do part 1 *)
  let _ = Printf.printf "Result: %d\n" (parseline data 0 0) in
  (* Part 2 *)
  let find_diff_by_ones lines =
    (* expensive method, but used only for printing result *)
    let string_of_char_union a b =
      String.of_seq (List.to_seq (List.map (fun (x,y) -> x) (List.filter (fun (x,y) -> x = y) (List.combine a b))))
    in
    let hamming_distance a b =
      let zipped = List.combine a b in
      let mismatches = List.filter (fun (x, y) -> x != y) zipped in
      List.length mismatches
    in
    let rec slide_linepairs (lines: char list list) =
      match lines with
      | [] -> ()
      | h :: [] -> ()
      | h1 :: h2 :: t ->
        let distance = hamming_distance h1 h2 in
        let _ = match distance with
          | 1 ->
            (* turn h1 and h2 from charlists to printable strings *)
            let a = String.of_seq (List.to_seq h1) in
            let b = String.of_seq (List.to_seq h2) in
            Printf.printf "Almost match: %s %s\n" a b;
            Printf.printf "Union %s\n" (string_of_char_union h1 h2)
          | _ -> ()
        in
        slide_linepairs (h2 :: t)
    in
    slide_linepairs data
  in
  find_diff_by_ones data
;;

let _ = day2;;
