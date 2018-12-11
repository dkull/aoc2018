open Base
open Stdio
open Poly
;;

let data =
  let line = Stdio.In_channel.input_line Stdio.stdin in
  Option.value_exn line
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
;;

let rec get_metas from id =
  let nr_child, nr_metas = List.nth_exn data (from+0), List.nth_exn data (from+1) in
  let child_from = ref (from + 2) in
  let childiter = List.range (id+1) (nr_child+id+1) in

  let child_ends, child_metas, child_values = List.unzip3 @@ List.map childiter (fun child_id ->
      let (m, e, v) = get_metas (!child_from) child_id in
      let _ = child_from := e in
      e, m, v
    )
  in

  let child_meta = List.fold child_metas ~init:0 ~f:(fun a v -> a+v) in
  let child_end =
  match List.last child_ends with
  | None -> child_from
  | Some x -> ref x
  in
  let my_end = !child_end + nr_metas in
  let mymetas = List.sub data !child_end (nr_metas) in
  let mymeta = Option.value_exn (List.reduce mymetas (+)) in
  let final_metas = mymeta + child_meta in

  let my_value = if nr_child = 0 then
      mymeta
    else
      List.fold ~init:0 ~f:(+) (List.map mymetas (fun x ->
          let value =
            match List.nth child_values (x-1) with
            | None -> 0
            | Some y -> y
          in
          value
        ))
  in
  let _ = Stdio.printf "%d ends_on: %d meta_sum %d value %d \n" id my_end final_metas my_value in
  (final_metas:int), (my_end:int), (my_value:int)
in
get_metas 0 0
