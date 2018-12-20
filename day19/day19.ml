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

let print_registers registers =
  let _ = Array.iter registers ~f:(fun r -> Stdio.printf "%d " r) in
  Stdio.printf "\n"
;;

let prev_0 = ref 0 ;;
let program_count = Array.length program ;;
let rec mainloop iteration registers ~ip =
  match ip with
  | ip when ip >= program_count || ip < 0 -> registers.(0)
  | _ ->
    let _ = program.(ip) ~regs: registers in
    let read_ip = registers.(ip_register) in
    let new_ip = read_ip + 1 in
    let _ = Array.set registers ip_register new_ip in
    mainloop (iteration+1) registers ~ip:new_ip
;;

let registers1 = Array.create ~len:6 0 in
Stdio.printf "Part1: %d\n" @@ mainloop 0 registers1 ~ip:0
;;

(* Did part 2 manually using factor and a calculator shell *)
(*
   Reg 3 contained 10551425
   Used factors to get: 5 5 422057
    >>> a = 5; b = 5; c=422057
    >>> 1 + a + (a*b) + c + (a*c) + 10551425
    13083798
*)

(* let registers2 = Array.create ~len:6 0 in
 * let _ = Array.set registers2 0 1 in
 * Stdio.printf "Part2: %d\n" @@ mainloop 0 registers2 ~ip:0 *)
