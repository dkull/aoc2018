open Base
open Stdio
open Set
;;

type pi = { mutable x: int ; mutable y: int ; xvec: int ; yvec: int } ;; 
module PointInfo = struct
  module T = struct
    type t = pi
    let compare t1 t2 =
      let c1 = Int.compare t1.x t2.x in
      let c2 = Int.compare t1.y t2.y in
      match c1 with
      | -1 -> -1
      | 1 -> 1
      | 0 -> c2
      | _ -> 0

    let sexp_of_t t : Sexp.t =
      List [ Int.sexp_of_t t.x ; Int.sexp_of_t t.y ]

  end
  include T
  include Comparator.Make(T)
end ;;

let filter_numbers line =
  let re_remove = Str.regexp "[a-z<>,=]+" in
  let re_trim = Str.regexp " +" in
  let cleaned_line = Str.global_replace re_remove " " line in
  let cleaned_line = Str.global_replace re_trim " " cleaned_line in
  let cleaned_line = Caml.String.trim cleaned_line in
  cleaned_line
;;

let rec get_data () =
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> []
  | Some line ->
    let parsed_line = line
      |> filter_numbers
      |> String.split ~on:' '
      |> List.map ~f:(fun x -> x |> String.strip |> Int.of_string)
      |> (fun i -> {
          x=    List.nth_exn i 0 ;
          y=    List.nth_exn i 1 ;
          xvec= List.nth_exn i 2 ;
          yvec= List.nth_exn i 3 ;
        })
    in
    parsed_line :: get_data ()
;;

let get_stats points =
  let left = Option.value_exn (List.reduce ~f:min (List.map points ~f:(fun x -> x.x))) in
  let right = Option.value_exn (List.reduce ~f:max (List.map points ~f:(fun x -> x.x))) in
  let top = Option.value_exn (List.reduce ~f:min (List.map points ~f:(fun x -> x.y))) in
  let bottom = Option.value_exn (List.reduce ~f:max (List.map points ~f:(fun x -> x.y))) in
  let width = right - left in
  let height = bottom - top in
  left, right, top, bottom, width, height
;;

let get_score points =
  let l,r,t,b,width,height = get_stats points in
  width + height
;;

let apply_vector ?(reverse=false) point =
  let op = if reverse then (-) else (+) in
  let _ = point.x <- op point.x point.xvec in
  let _ = point.y <- op point.y point.yvec in
  ()
;;

let create_lookuptable pts =
  Set.of_list (module PointInfo) pts
;;

let draw_points points =
  let lookup = create_lookuptable points in
  let _ = Stdio.printf "Ceated dict with %d items\n" (Set.length lookup)
  in
  let l, r, t, b, w, h = get_stats points in
  let rec draw_cell xfrom xat xto yfrom yat yto =
    let lookupkey = {x=xat; y=yat; xvec=0; yvec=0} in
    let _ = match Set.mem lookup lookupkey with
    | true -> Stdio.printf "X"
    | false -> Stdio.printf "."
    in
    let newcoords = match (xat, yat) with
    | (x, y) when x = xto && y = yto -> None
    | (x, _) when x = xto ->
      let _ = Stdio.printf "\n" in
      Some(xfrom, yat + 1)
    | _ -> Some(xat+1, yat)
    in
    match newcoords with
    | None ->
      let _ = Stdio.printf "\n" in
      ()
    | Some (x, y) ->
      draw_cell xfrom x xto yfrom y yto
  in
  draw_cell l l r t t b
;;

let data = get_data () in
let main =
  let rec second ?(bestscore) ~time:secs () =
    let has_bestscore = Option.is_some bestscore in
    let score = get_score data in
    let _ = Stdio.printf "Dimsum: %d %d\n" secs score in
    let new_bestscore = match has_bestscore with
      | false -> score
      | true ->
        if score < Option.value_exn bestscore then
          score
        else
          let _ = List.map data ~f:(apply_vector ~reverse:true) in
          let _ = draw_points data in
          let _ = List.map data ~f:(apply_vector ~reverse:false) in
          Caml.exit 0
    in
    let _ = List.map data ~f:apply_vector in
    second ~bestscore:new_bestscore ~time:(secs+1) ()
  in
  second ~time:0 ()
in
main
