open Base
open Stdio
;;

let op_addr ~regs a b c = Array.set regs c @@
  (+) regs.(a) regs.(b)
let op_addi ~regs a b c = Array.set regs c @@
  (+) regs.(a) b
let op_mulr ~regs a b c = Array.set regs c @@
  regs.(a) * regs.(b)
let op_muli ~regs a b c = Array.set regs c @@
  regs.(a) * b
let op_banr ~regs a b c = Array.set regs c @@
  regs.(a) land regs.(b)
let op_bani ~regs a b c = Array.set regs c @@
  regs.(a) land b
let op_borr ~regs a b c = Array.set regs c @@
  regs.(a) lor regs.(b)
let op_bori ~regs a b c = Array.set regs c @@
  regs.(a) lor b
let op_setr ~regs a b c = Array.set regs c @@
  regs.(a)
let op_seti ~regs a b c = Array.set regs c @@
  a
let op_gtir ~regs a b c = Array.set regs c @@
  if a > regs.(b) then 1 else 0
let op_gtri ~regs a b c = Array.set regs c @@
  if regs.(a) > b then 1 else 0
let op_gtrr ~regs a b c = Array.set regs c @@
  if regs.(a) > regs.(b) then 1 else 0
let op_eqir ~regs a b c = Array.set regs c @@
  if a = regs.(b) then 1 else 0
let op_eqri ~regs a b c = Array.set regs c @@
  if regs.(a) = b then 1 else 0
let op_eqrr ~regs a b c = Array.set regs c @@
  if regs.(a) = regs.(b) then 1 else 0

let op_lookup = Hashtbl.of_alist_exn (module String) [
  ("addr",  op_addr) ;
  ("addi",  op_addi) ;
  ("mulr",  op_mulr) ;
  ("muli",  op_muli) ;
  ("banr",  op_banr) ;
  ("bani",  op_bani) ;
  ("borr",  op_borr) ;
  ("bori",  op_bori) ;
  ("setr",  op_setr) ;
  ("seti",  op_seti) ;
  ("gtir",  op_gtir) ;
  ("gtri",  op_gtri) ;
  ("gtrr",  op_gtrr) ;
  ("eqir",  op_eqir) ;
  ("eqri",  op_eqri) ;
  ("eqrr",  op_eqrr) ;
]
;;

type prog_line = (int Array.t -> int -> int -> int -> int) * int * int * int ;;

let input_lines = Stdio.In_channel.input_lines Stdio.stdin  ;;
let header, program_lines = match input_lines with
  | h :: t -> h, t
  | _ -> raise @@ Failure "Bad input"
;;

let line_lookup = Array.of_list input_lines ;;

let ip_register = Int.of_string @@ List.nth_exn (String.split ~on:' ' header) 1 ;;
let program =
  List.map program_lines ~f:(fun l ->
      let tokens = String.split ~on:' ' l in
      let op = List.nth_exn tokens 0 in
      let a = Int.of_string @@ List.nth_exn tokens 1 in
      let b = Int.of_string @@ List.nth_exn tokens 2 in
      let c = Int.of_string @@ List.nth_exn tokens 3 in
      let op_fun = Hashtbl.find_exn op_lookup op in
      op_fun a b c
    )
  |> Array.of_list
;;

let print_state ip registers =
  let _ = Stdio.printf "%17.s --> " line_lookup.(ip) in
  let _ = Array.iteri registers ~f:(fun i r -> Stdio.printf "%d:%7.d " i r) in
  Stdio.printf "\n"
;;

let part1_done = ref false ;;
let seen_0s = Hashtbl.create (module Int) ;;
let program_count = Array.length program ;;
let rec mainloop iteration registers ~ip =
  let reg3 = registers.(3) in
  let _ = if ip = 28 && not (Hashtbl.mem seen_0s reg3) then
      let _ = if not !part1_done then
          let _ = part1_done := true in
          Stdio.printf "Part1: %d\n" reg3
      in
      let _ = Stdio.printf "Found %d\n " reg3 in
      let _ = Hashtbl.set seen_0s reg3 true in
      ()
  in
  let _ = program.(ip) ~regs: registers in
  let read_ip = registers.(ip_register) in
  let new_ip = read_ip + 1 in
  let _ = Array.set registers ip_register new_ip in
  mainloop (iteration+1) registers ~ip:new_ip
;;

let registers1 = Array.create ~len:6 0 in
mainloop 0 registers1 ~ip:0
