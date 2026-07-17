The compiler's libraries (stdlib, unix, str, ...) have no META, so they
never appear in Findlib.all_packages; the remap mapping for the compiler
package is injected unconditionally, regardless of what this trivial
package's docs reference.

  $ dune build @doc

Exactly one compiler-package mapping is injected (the name varies by OCaml
version, so match either form):

  $ grep -cE '^ocaml(-base)?-compiler/:https://ocaml\.org/p/ocaml(-base)?-compiler/' \
  >   _build/default/_doc/_remap/remap.txt
  1

It is the one selected by the >= 5.3.0 heuristic for THIS compiler:

  $ maj=$(ocamlc -version | cut -d. -f1)
  $ min=$(ocamlc -version | cut -d. -f2)
  $ if [ "$maj" -gt 5 ] || { [ "$maj" -eq 5 ] && [ "$min" -ge 3 ]; }; \
  >   then exp=ocaml-compiler; else exp=ocaml-base-compiler; fi
  $ grep -q "^$exp/:https://ocaml.org/p/$exp/" _build/default/_doc/_remap/remap.txt \
  >   && echo ok
  ok

The findlib library is never emitted under its findlib name (it is either
overridden to ocamlfind or absent):

  $ grep -c '^findlib/:' _build/default/_doc/_remap/remap.txt
  0
  [1]
