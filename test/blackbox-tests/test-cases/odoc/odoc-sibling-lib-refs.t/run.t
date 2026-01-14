Test that libraries in the same package can reference each other in documentation
even without compile-time dependencies.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (name mypkg)
  > (package (name mypkg))
  > EOF

Create two libraries in the same package that don't depend on each other:

  $ mkdir liba libb

  $ cat > liba/dune << EOF
  > (library
  >  (name liba)
  >  (public_name mypkg.liba))
  > EOF

  $ cat > liba/liba.mli << EOF
  > (** Library A.
  >     See also {!Libb} for related functionality. *)
  > 
  > val foo : unit -> unit
  > EOF

  $ cat > liba/liba.ml << EOF
  > let foo () = ()
  > EOF

  $ cat > libb/dune << EOF
  > (library
  >  (name libb)
  >  (public_name mypkg.libb))
  > EOF

  $ cat > libb/libb.mli << EOF
  > (** Library B.
  >     See also {!Liba} for related functionality. *)
  > 
  > val bar : unit -> unit
  > EOF

  $ cat > libb/libb.ml << EOF
  > let bar () = ()
  > EOF

Build the documentation - should succeed without unresolved reference warnings:

  $ dune build @doc 2>&1 | grep -i "unresolved\|warning" || echo "No warnings"
  No warnings

Verify both libraries are documented:

  $ find _build/default/_doc/_html -name "index.html" | grep -E "Liba|Libb" | sort
  _build/default/_doc/_html/mypkg/mypkg.liba/Liba/index.html
  _build/default/_doc/_html/mypkg/mypkg.libb/Libb/index.html
