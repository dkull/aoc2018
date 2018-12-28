# aoc2018
Learning OCaml with Advent Of Code 2018

# Compile for performance:
In dir with eg. day25.ml:
(produces day25.native executable)
```
ocamlbuild -ocamlopt "ocamlopt -O3 -unsafe -rounds 10 " -use-menhir -package stdio,base day25.native
```

# Simplest compile:
```
ocamlbuild -package stdio,base day25.native
```

# Another way
(optional flags: -g -O3 -p, etc)
(produces a.out executable)
```
ocamlfind ocamlopt -O3 -linkpkg -package base,stdio day25.ml
```

# All input is read from stdin:
cat day25.input | ./day25.native
