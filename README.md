# aoc2018
Learning OCaml with Advent Of Code 2018

# In dir with eg. day23.ml:
ocamlbuild -ocamlopt "ocamlopt -O3 -unsafe" -use-menhir -package stdio,base day23.native && cat day23.input | ./day23.native

# Or minimally: 
ocamlbuild -package stdio,base day23.native
cat day23.input | ./day23.native

# Another way
# optional: -g -O3 -p, etc
ocamlfind ocamlopt -O3 -linkpkg -package base,stdio day21.ml

# If solution needs input it needs to given in stdin, eg:
cat day21.input | time ./a.out
