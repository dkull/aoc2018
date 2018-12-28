open Base
open Stdio

module XyHashtbl = struct
  type t = int * int

  let hash = Hashtbl.hash

  let compare a b =
    let ax, ay = a in
    let bx, by = b in
    if ax = bx then compare ay by else compare ax bx

  let sexp_of_t t : Sexp.t = List [Int.sexp_of_t (fst t); Int.sexp_of_t (snd t)]
end

let depth =
  In_channel.input_line_exn Stdio.stdin
  |> String.split ~on:' ' |> List.last_exn |> Int.of_string

let target_xy =
  In_channel.input_line_exn Stdio.stdin
  |> String.split ~on:' ' |> List.last_exn |> String.split ~on:','
  |> function
  | [x; y] -> (Int.of_string x, Int.of_string y)
  | _ -> raise @@ Failure "Bad input"

type geology = Rocky | Wet | Narrow

(* upleft used only in part1 to determine top-left corner *)
type direction = Left | Up | Down | Right | UpLeft

let translate_coord coord direction =
  match direction with
  | Left -> (fst coord - 1, snd coord)
  | Up -> (fst coord, snd coord - 1)
  | Down -> (fst coord, snd coord + 1)
  | Right -> (fst coord + 1, snd coord)
  | UpLeft -> raise @@ Failure "Unnecessary translation"

let print_risk map =
  let risk = ref 0 in
  Array.iteri map ~f:(fun y row ->
      Array.iteri row ~f:(fun x col ->
          if x <= fst target_xy && y <= snd target_xy then
            match col with
            | Rocky -> risk := !risk + 0
            | Wet -> risk := !risk + 1
            | Narrow -> risk := !risk + 2
          else () ) ) ;
  printf "Part1 %d\n" !risk

let out_of_bounds coord =
  if fst coord < 0 then true else if snd coord < 0 then true else false

let get_region_type erosion_level =
  match erosion_level % 3 with
  | 0 -> Rocky
  | 1 -> Wet
  | 2 -> Narrow
  | _ -> raise @@ Failure "Math is broken"

type claim_data =
  {tile: geology; mutable hands: bool; mutable torch: bool; mutable gear: bool}

let claims = Hashtbl.create ~size:10000000 (module XyHashtbl)

let new_claim erosion =
  match erosion with
  | Rocky -> {tile= erosion; hands= true; gear= false; torch= false}
  | Wet -> {tile= erosion; hands= false; gear= false; torch= true}
  | Narrow -> {tile= erosion; hands= false; gear= true; torch= false}

let generate_maps dim_mult depth target_xy =
  let x_plus1 = fst target_xy + 1 in
  let y_plus1 = snd target_xy + 1 in
  let geo_indexes =
    Array.make_matrix ~dimx:(y_plus1 * dim_mult) ~dimy:(x_plus1 * dim_mult)
      (-1)
  in
  let erosion_levels =
    Array.make_matrix ~dimx:(y_plus1 * dim_mult) ~dimy:(x_plus1 * dim_mult)
      (-1)
  in
  let region_type =
    Array.make_matrix ~dimx:(y_plus1 * dim_mult) ~dimy:(x_plus1 * dim_mult)
      Rocky
  in
  let rec traverse depth current_coord =
    let x, y = current_coord in
    match out_of_bounds current_coord with
    | true -> ()
    | false -> (
        let existing_geo_index = geo_indexes.(y).(x) in
        match existing_geo_index with
        | gi when gi >= 0 -> ()
        | _ ->
            let () = traverse depth (translate_coord current_coord Left) in
            let () = traverse depth (translate_coord current_coord Up) in
            let geo_index =
              match current_coord with
              | 0, 0 -> 0
              | _ when x = fst target_xy && y = snd target_xy -> 0
              | _ when y = 0 -> x * 16807
              | _ when x = 0 -> y * 48271
              | _ ->
                  let left_erosion = erosion_levels.(y).(x - 1) in
                  let top_erosion = erosion_levels.(y - 1).(x) in
                  left_erosion * top_erosion
            in
            let erosion_level = (geo_index + depth) % 20183 in
            geo_indexes.(y).(x) <- geo_index ;
            erosion_levels.(y).(x) <- erosion_level ;
            region_type.(y).(x) <- get_region_type erosion_level ;
            if fst target_xy = x && snd target_xy = y then
              Hashtbl.set claims (x, y) (new_claim Rocky)
            else
              Hashtbl.set claims (x, y)
                (new_claim (get_region_type erosion_level)) ;
            () )
  in
  let bottom_right_corner =
    (fst target_xy * dim_mult, snd target_xy * dim_mult)
  in
  let _ = traverse depth bottom_right_corner in
  region_type

(* generate maps *)
let region = generate_maps 2 depth target_xy

(* calc+print part1 *)

;;
print_risk region

(* part2 stuff *)
type tool = Torch | Gear | Hands
let orientations = [Right; Left; Up; Down]

type walker_status =
  { mutable minutes: int
  ; tool: tool
  ; mutable steps_taken: int
  ; path: (int * int * int * tool) list }

let tool_swap tile_from tile_to =
  match (tile_from, tile_to) with
  | Rocky, Rocky -> None
  | Rocky, Wet -> Some Gear
  | Wet, Rocky -> Some Gear
  | Wet, Wet -> None
  | Wet, Narrow -> Some Hands
  | Narrow, Wet -> Some Hands
  | Narrow, Narrow -> None
  | Narrow, Rocky -> Some Torch
  | Rocky, Narrow -> Some Torch

let expand_walker claims walkers unclaimed walker tile =
  List.iter unclaimed ~f:(fun xy ->
      let x, y = xy in
      let unclaimed_tile = Hashtbl.find_exn claims xy in
      let change_tool_to = tool_swap tile unclaimed_tile.tile in
      let new_walker =
        match change_tool_to with
        | None ->
            { steps_taken= walker.steps_taken + 1
            ; minutes= 0
            ; tool= walker.tool
            ; path= (x, y, walker.steps_taken + 1, walker.tool) :: walker.path
            }
        | Some tool ->
            let time_spent =
              match (tool, walker.tool) with
              | Gear, Gear -> 0
              | Hands, Hands -> 0
              | Torch, Torch -> 0
              | _ -> 7
            in
            { steps_taken= walker.steps_taken + time_spent + 1
            ; minutes= time_spent
            ; tool
            ; path=
                (x, y, walker.steps_taken + time_spent + 1, tool)
                :: walker.path }
      in
      let existing_walkers = Hashtbl.find walkers xy in
      match existing_walkers with
      | Some w ->
          let unique =
            List.filter w ~f:(fun w ->
                let same =
                  match (w.tool, new_walker.tool) with
                  | Gear, Gear | Hands, Hands | Torch, Torch ->
                      w.minutes >= new_walker.minutes
                      && w.steps_taken >= new_walker.steps_taken
                  | _ -> false
                in
                not same )
          in
          Hashtbl.set walkers xy (new_walker :: unique)
      | None -> Hashtbl.set walkers xy [new_walker] )

let claim_tile exploration xy tool =
  let data = Hashtbl.find_exn exploration xy in
  let () =
    match tool with
    | Gear -> data.gear <- true
    | Torch -> data.torch <- true
    | hands -> data.hands <- true
  in
  ()

let unclaimed_neighbors claims xy =
  List.filter_map orientations ~f:(fun orientation ->
      let new_xy = translate_coord xy orientation in
      let not_oob = not @@ out_of_bounds new_xy in
      match not_oob with
      | false -> None
      | true -> (
          let claim = Hashtbl.find claims new_xy in
          match claim with
          | None -> None
          | Some claim ->
              let not_claimed =
                match claim.tile with
                | Rocky -> (not claim.gear) || not claim.torch
                | Wet -> (not claim.hands) || not claim.gear
                | Narrow -> (not claim.torch) || not claim.hands
              in
              if not_claimed then Some new_xy else None ) )

let tick_walker claims walkers xy walker =
  let claim = Hashtbl.find_exn claims xy in
  let tile = claim.tile in
  let claimed =
    match walker.tool with
    | Gear -> claim.gear
    | Hands -> claim.hands
    | Torch -> claim.torch
  in
  match claimed with
  | true -> false
  | false -> (
    match walker.minutes with
    | 0 ->
        let unclaimed = unclaimed_neighbors claims xy in
        claim_tile claims xy walker.tool ;
        expand_walker claims walkers unclaimed walker tile ;
        false
    | _ ->
        walker.minutes <- walker.minutes - 1 ;
        true )

let remove_walker ws xy w =
  Hashtbl.set ws xy
    ( Hashtbl.find_exn ws xy
    |> List.filter ~f:(fun x ->
           not
             ( match (w.tool, x.tool) with
             | Gear, Gear | Hands, Hands | Torch, Torch ->
                 w.steps_taken = x.steps_taken && w.minutes = x.minutes
             | _ -> false ) ) )

;;
let walkers = Hashtbl.create (module XyHashtbl) in
let rec walk iteration region () =
  let curr_walkers = Hashtbl.to_alist walkers in
  let () =
    List.iter curr_walkers ~f:(fun (k, v) ->
        List.iter v ~f:(fun walker ->
            let alive = tick_walker claims walkers k walker in
            let () =
              if fst k = fst target_xy && snd k = snd target_xy && not alive
              then
                let result = match walker.tool with
                  | Torch -> walker.steps_taken
                  | _ -> walker.steps_taken + 7
                in
                let () = printf "Part2: %d\n" result in
                Caml.exit 0
            in
            match alive with
            | false -> remove_walker walkers k walker
            | true -> () ) )
  in
  walk (iteration + 1) region ()
in
(* add first walker *)
let _ =
  Hashtbl.set walkers (0, 0)
    [{minutes= 0; tool= Torch; steps_taken= 0; path= []}]
in
walk 0 region ()
