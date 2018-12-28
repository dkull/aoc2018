open Base
open Stdio

let input =
  In_channel.input_lines Stdio.stdin
  |> List.map ~f:(fun line ->
         String.substr_replace_all line ~pattern:"pos=<" ~with_:""
         |> String.substr_replace_all ~pattern:">, r=" ~with_:","
         |> String.split ~on:',' |> List.map ~f:Int.of_string
         |> function
         | [h1; h2; h3; h4] -> (h1, h2, h3, h4)
         | _ -> raise @@ Failure "Bad input" )

let manhattan a b =
  let a1, a2, a3 = a in
  let b1, b2, b3 = b in
  (abs @@ (a1 - b1)) + (abs @@ (a2 - b2)) + (abs @@ (a3 - b3))

let bestpos, best_range, in_range =
  let sorted =
    List.sort input ~compare:(fun a b ->
        let _, _, _, rangea = a in
        let _, _, _, rangeb = b in
        compare rangeb rangea )
  in
  List.fold sorted
    ~init:((0, 0, 0), 0, 0)
    ~f:(fun acc item ->
      let x, y, z, range = item in
      let best_pos, best_range, in_range = acc in
      if best_range = 0 then ((x, y, z), range, 1)
      else if manhattan best_pos (x, y, z) <= best_range then
        (best_pos, best_range, in_range + 1)
      else acc )

;;
printf "Part1: %d\n" in_range

(* part 2 *)

let scale_point scale point =
  let a, b, c, d = point in
  let a, b, c, d =
    (Float.of_int a, Float.of_int b, Float.of_int c, Float.of_int d)
  in
  let a, b, c, d = (a /. scale, b /. scale, c /. scale, d /. scale) in
  let a, b, c, d =
    ( Float.round_down a
    , Float.round_down b
    , Float.round_down c
    , Float.round_up d )
  in
  let a, b, c, d =
    (Int.of_float a, Int.of_float b, Int.of_float c, Int.of_float d)
  in
  (a, b, c, d)

let find_boundary scaled_points =
  List.fold scaled_points ~init:(100, -100, 100, -100, 100, -100)
    ~f:(fun acc sphere ->
      let x, y, z, r = sphere in
      let xmin, xmax, ymin, ymax, zmin, zmax = acc in
      let xmin = min x xmin in
      let xmax = max x xmax in
      let ymin = min y ymin in
      let ymax = max y ymax in
      let zmin = min z zmin in
      let zmax = max z zmax in
      (xmin, xmax, ymin, ymax, zmin, zmax) )

let point_in_sphere point sphere =
  let sx, sy, sz, sr = sphere in
  let distance = manhattan point (sx, sy, sz) in
  distance <= sr

let iter_coords boundary =
  let xmin, xmax, ymin, ymax, zmin, zmax = boundary in
  let xrange = Sequence.range ~start:`inclusive ~stop:`exclusive xmin xmax in
  let yrange = Sequence.range ~start:`inclusive ~stop:`exclusive ymin ymax in
  let zrange = Sequence.range ~start:`inclusive ~stop:`exclusive zmin zmax in
  let a = Sequence.cartesian_product xrange yrange in
  let b = Sequence.cartesian_product a zrange in
  b

let best_point boundary spheres =
  let boundary_coords = iter_coords boundary in
  let best_point =
    Sequence.fold boundary_coords
      ~init:(0, (0, 0, 0))
      ~f:(fun acc point ->
        let (x, y), z = point in
        let sphere_count =
          List.count spheres ~f:(fun sphere -> point_in_sphere (x, y, z) sphere)
        in
        match sphere_count >= fst acc with
        | false -> acc
        | true ->
            if sphere_count = fst acc then
              let current = manhattan (x, y, z) (0, 0, 0) in
              let prev = manhattan (snd acc) (0, 0, 0) in
              if current < prev then (sphere_count, (x, y, z)) else acc
            else (sphere_count, (x, y, z)) )
  in
  best_point

let print_point best =
  let best_score, (x, y, z) = best in
  printf "Best point %d,%d,%d @ %d\n" x y z best_score

let print_boundary boundary =
  let x1, x2, y1, y2, z1, z2 = boundary in
  let contains = (x2 - x1) * (y2 - y1) * (z2 - z1) in
  printf "Boundary %d to %d, %d to %d, %d to %d contains %d\n" x1 x2 y1 y2 z1
    z2 contains

let point_to_boundary point =
  let x, y, z = point in
  let x, xto, y, yto, z, zto =
    ( x * 10
    , ((x + 1) * 10) - 1
    , y * 10
    , ((y + 1) * 10) - 1
    , z * 10
    , ((z + 1) * 10) - 1 )
  in
  let boundary = (x, xto, y, yto, z, zto) in
  boundary

;;
let rec scaler scale points ?boundary () =
  let () = printf "\n" in
  let () = printf "Scaling with %.0f\n" scale in
  let rec by_point points scaled =
    match points with
    | [] -> scaled
    | h :: t ->
        let a, b, c, d = scale_point scale h in
        by_point t ((a, b, c, d) :: scaled)
  in
  let scaled_points = by_point points [] in
  let boundary =
    match boundary with None -> find_boundary scaled_points | Some b -> b
  in
  let () = print_boundary boundary in
  let () = Out_channel.flush Stdio.stdout in
  let best = best_point boundary scaled_points in
  let () = print_point best in
  match Int.of_float scale with
  | a when a <= 1 ->
      let () = printf "Distance: %d\n" (manhattan (snd best) (0, 0, 0)) in
      ()
  | _ ->
      let best_score, best_pnt = best in
      let new_scale = scale /. 10.0 in
      let new_boundary = point_to_boundary best_pnt in
      let () = print_boundary new_boundary in
      scaler new_scale points ~boundary:new_boundary ()
in
scaler 100000000.0 input ()
