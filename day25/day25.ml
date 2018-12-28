open Base
open Stdio

let lines = In_channel.input_lines Stdio.stdin

type point = int * int * int * int

let manhattan a b =
  let a1, a2, a3, a4 = a in
  let b1, b2, b3, b4 = b in
  abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3) + abs (a4 - b4)

class constellation init =
  object
    val mutable elems = init

    method belongs elem =
      let linker =
        List.find elems ~f:(fun existing_elem ->
            let distance = manhattan elem existing_elem in
            distance <= 3 )
      in
      match linker with None -> false | Some _ -> true

    method force_elem elem = elems <- elem :: elems

    method get_elems = elems

    method merge (constellations : constellation list) =
      List.iter constellations ~f:(fun other_const ->
          let const_elems = other_const#get_elems in
          List.iter const_elems ~f:(fun point -> elems <- point :: elems) ;
          other_const#delete_all )

    method in_use = not @@ List.is_empty elems

    method delete_all = elems <- []
  end

let data =
  lines
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.map ~f:(fun tokens ->
         ( List.nth_exn tokens 0
         , List.nth_exn tokens 1
         , List.nth_exn tokens 2
         , List.nth_exn tokens 3 ) )

;;
let rec group_lines points constellations =
  match points with
  | [] -> constellations
  | point :: t_points -> (
      let belongs_to =
        List.filter constellations ~f:(fun const -> const#belongs point)
      in
      match belongs_to with
      | [] ->
          let constellation = new constellation [point] in
          group_lines t_points (constellation :: constellations)
      | h_const :: t_consts ->
          let () = h_const#merge t_consts in
          let () = h_const#force_elem point in
          let cleaned_constellations =
            List.filter constellations ~f:(fun const -> const#in_use)
          in
          group_lines t_points cleaned_constellations )
in
let output = group_lines data [] in
printf "Part1: %d\n" (List.length output)
