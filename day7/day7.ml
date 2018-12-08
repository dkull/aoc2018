(*
   Compile:
   > ocamlfind ocamlopt -linkpkg -package base,stdio day7.ml
   Run:
   > cat day7.input | ./a.out
*)

open Base
open Stdio
open Poly

type work = {step: char; timeleft: int}

let blockedby =
  let blockedby_map = Hashtbl.create (module Char) in
  let _ =
    Stdio.In_channel.input_lines Stdio.stdin
    |> List.sort ~compare:compare_string
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(fun x ->
           ( Char.of_string (List.nth_exn x 1)
           , Char.of_string (List.nth_exn x 7) ) )
    |> List.iter ~f:(fun x ->
        let _ = Hashtbl.update blockedby_map (snd x) (fun v ->
            match v with
            | None -> Set.of_list (module Char) [fst x]
            | Some y ->  Set.add y (fst x))
        in
        let _ = Hashtbl.update blockedby_map (fst x) (fun v ->
           match v with
           | None -> Set.of_list (module Char) []
           | Some x -> x
          )
        in
        () )
  in
  blockedby_map

let worker_pool = Hashtbl.create (module Char) 

let assemble ?(workers = 1) () =
  let rec check_unblocked timepassed () =
    let _ = Hashtbl.keys worker_pool |> List.iter ~f:(fun key ->
        Hashtbl.decr ~remove_if_zero:false worker_pool key
      )
    in

    let just_finished = Hashtbl.filteri worker_pool (fun ~key:k ~data:v -> v = 0) in
    (* remove blocks from others *)
    let _ =
      let fin_keys = Hashtbl.keys just_finished in
      List.iter fin_keys (fun x ->
          Hashtbl.mapi_inplace blockedby (fun ~key:d ~data:v ->
            Set.remove v x
        )
      )
    in
    (* remove from worker pool *)
    let _ = Hashtbl.filter_keys_inplace worker_pool (fun d ->
        let willremove = Hashtbl.mem just_finished d in
        let _ = if willremove then
            Stdio.printf "%c" d
          else
            ()
        in
        not willremove
    ) in

    let can_do =
      let need_doing = List.sort compare_char (Hashtbl.keys blockedby) in
      List.filter need_doing (fun x ->
          let blocks = Hashtbl.find_exn blockedby x |> Set.length in
          blocks = 0
        )
    in

    let free_workers = workers - (List.length (Hashtbl.keys worker_pool)) in
    let new_job_count = min free_workers (List.length can_do) in

    let _ = List.take can_do new_job_count
          |> List.iter ~f:(fun s ->
            let _ = Hashtbl.add_exn worker_pool s ((Char.to_int s) - 4) in
            let _ = Hashtbl.remove blockedby s in
            ()
          )
    in
    let work_count = List.length @@ Hashtbl.keys worker_pool in
    match work_count = 0 with
    | true -> ()
    | false ->
      check_unblocked (timepassed + 1) ()
  in
  let _ = check_unblocked 0 () in
  Stdio.printf "\n"

;;
assemble ~workers:5 ()
