open Base
open Stdio
;;

type marble_ring = {
  value: int ;
  mutable ccw: marble_ring option;
  mutable cw: marble_ring option
} ;;

let rec insert current where what =
  match where with
  | 0 ->
    let mccw = Option.value_exn current.ccw in
    let _ = what.cw <- Some current in
    let _ = what.ccw <- current.ccw in
    let _ = mccw.cw <- Some what in
    let _ = current.ccw <- Some what in
    what
  | _ ->
    insert (Option.value_exn current.cw) (where - 1) what
in

let rec special current where extrascore =
  match where with
  | 0 ->
    let points = current.value in
    let prev = Option.value_exn current.ccw in
    let new_current = Option.value_exn current.cw in
    let _ = prev.cw <- Some new_current in
    let _ = new_current.ccw <- current.ccw in
    new_current, points + extrascore
  | _ ->
    special (Option.value_exn current.ccw) (where - 1) extrascore
in

let end_score = 7095300 in
let nr_elves = 405 in
let scoreboard = Hashtbl.create (module Int) ~size:nr_elves in
let rec round next_marble_val elf current_marble =
  let fresh_marble = { value= next_marble_val ; ccw= None ; cw= None } in
  let new_marble, points =
    match fresh_marble with
    | fm when fm.value % 23 = 0 ->
      special current_marble 7 fm.value
    | _ as fm ->
      let marble = insert current_marble 2 fm in
      marble, 0
  in
  let _ = Hashtbl.incr scoreboard elf ~by:points in
  let _ = if next_marble_val = end_score + 1 then
      let highest_score = Option.value_exn (List.reduce ~f:max (Hashtbl.data scoreboard)) in
      let _ = Stdio.printf "Game ended with winner score: %d\n" highest_score in
      Caml.exit 0
    else
      ()
  in
  round (next_marble_val + 1) (elf % nr_elves + 1) new_marble
in
let (zero_marble: marble_ring) =
  let m = {value= 0 ; ccw= None ; cw= None} in
  let _ = m.ccw <- Some m in
  let _ = m.cw <- Some m in
  m
in
round 1 1 zero_marble
