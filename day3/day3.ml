type patch = { id : int; x: int; y: int; w: int; h: int } ;;
type coords = {x: int ; y: int} ;;

let day2 =
  let load_input =
    let rec read_input got =
      try
        let line = read_line () in
        read_input @@ line :: got
      with End_of_file -> List.rev got
    in
    read_input []
  in
  (*let fabric = Array.make_matrix 1000 1000 0 in *)
  let fabric = Hashtbl.create 1000000 in
  let valid_cuts = Hashtbl.create 2000 in
  let cleaner_re = Str.regexp "[@:x;,#]" in
  let trim_re = Str.regexp " +" in
  let patch_of_line line =
    let cleaned_line = Str.global_replace cleaner_re " " line in
    let cleaned_line = Str.global_replace trim_re " " cleaned_line in
    let cleaned_line = String.trim cleaned_line in
    let tokens = String.split_on_char ' ' cleaned_line in
    let values = List.map (fun x -> int_of_string x) tokens in
    {
      id=List.nth values 0;
       x=List.nth values 1;
       y=List.nth values 2;
       w=List.nth values 3;
       h=List.nth values 4;
    }
  in
  let generate_coords patch =
    let maxindex = (patch.w * patch.h) - 1 in
    let rec inner coords index =
      match index with
      | -1 -> coords
      | _ ->
        let x = (index mod patch.w) + patch.x in
        let y = (index / patch.w) + patch.y in
        inner ({x;y} :: coords) (index - 1)
    in
    inner [] maxindex
  in
  let mark_fabric fabric patch_id coords valids =
    let rec inner coords overlaps =
      match coords with
      | [] -> overlaps
      | h :: t ->
        let marks =
          try
            Hashtbl.find fabric h
          with Not_found -> []
        in
        let overlap =
          match List.length marks with
          | 0 -> false
          | _ -> true
        in
        let new_overlap =
          match List.length marks with
          | 1 -> true
          | _ -> false
        in
        (* part 2 - remove all conflicting patches *)
        let _ = if overlap then
          let _ = Hashtbl.remove valids patch_id in
          let _ = List.map (fun x -> Hashtbl.remove valids x) marks in
          0
        else 0 in
        let _ = Hashtbl.replace fabric h (patch_id :: marks) in
        inner t (if new_overlap then overlaps + 1 else overlaps)
    in
    inner coords 0
  in
  let rec draw_patches fabric patches overlap_count =
    match patches with
    | [] -> overlap_count
    | h :: t ->
      let patch = patch_of_line h in
      (* part 2 - write all ids to hashtable,
         we remove the ones that are participants in collision *)
      let _ = Hashtbl.replace valid_cuts patch.id true in
      let coords = generate_coords patch in
      let patch_id = patch.id in
      let new_overlaps = mark_fabric fabric patch_id coords valid_cuts in
      draw_patches fabric t (overlap_count + new_overlaps)
  in
  let overlaps = draw_patches fabric load_input 0 in
  let conflict_frees = List.of_seq (Hashtbl.to_seq_keys valid_cuts) in
  let _ = Printf.printf "Clean %d of %d\n" (List.nth conflict_frees 0) (List.length conflict_frees) in
  Printf.printf "Overlaps: %d\n" overlaps
;;

let _ = day2 ;;
