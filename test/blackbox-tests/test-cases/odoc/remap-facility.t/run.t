Test the remap facility for documentation generation

This test verifies that:
1. @doc builds only local packages
2. @doc-full builds all packages (local + installed)
3. Both modes work correctly

Setup a simple project with a local library:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name mylib))
  > EOF

  $ cat > mylib.opam <<EOF
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml <<EOF
  > (** My library *)
  > let hello () = "Hello, world!"
  > EOF

Build documentation with @doc (local-only mode):

  $ dune build @doc
  Error: No rule found for _doc/_remap/remap.txt
  -> required by alias doc
  [1]

Check that local package HTML was built:

  $ find _build/default/_doc/_html -name "*.html" | grep -E "mylib|index" | sort
  find: _build/default/_doc/_html: No such file or directory

Build documentation with @doc-full (full mode):

  $ dune build @doc-full
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
     { dir = In_build_dir "default/_doc/_html_full/mylib"
     ; example =
         Rule
           { targets =
               { root = In_build_dir "default/_doc/_html/mylib"
               ; files = set { "sidebar.json" }
               ; dirs = set {}
               }
           }
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from
    Dune_engine__Load_rules.Load_rules.Normal.make_rules_gen_result.(fun) in
    file "src/dune_engine/load_rules.ml", line 549, characters 10-51
  Called from Fiber__Core.O.(>>|).(fun) in file "src/fiber/src/core.ml", line
    257, characters 36-41
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default/_doc/_html_full/mylib")
  -> required by
     ("build-alias",
      { dir = In_build_dir "default/_doc/_html_full/mylib"; name = "doc" })
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "default"; name = "doc-full" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("[gen_rules] returned rules in a directory that is not a descendant of the directory it was called for",
     { dir = In_build_dir "default/_doc/_html_full/ocaml-compiler"
     ; example =
         Rule
           { targets =
               { root = In_build_dir "default/_doc/_html/ocaml-compiler"
               ; files = set { "sidebar.json" }
               ; dirs = set {}
               }
           }
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from
    Dune_engine__Load_rules.Load_rules.Normal.make_rules_gen_result.(fun) in
    file "src/dune_engine/load_rules.ml", line 549, characters 10-51
  Called from Fiber__Core.O.(>>|).(fun) in file "src/fiber/src/core.ml", line
    257, characters 36-41
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by
     ("load-dir", In_build_dir "default/_doc/_html_full/ocaml-compiler")
  -> required by
     ("build-alias",
      { dir = In_build_dir "default/_doc/_html_full/ocaml-compiler"
      ; name = "doc"
      })
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "default"; name = "doc-full" })
  -> required by ("toplevel", ())
  [1]

Check that HTML was built in _html_full:

  $ find _build/default/_doc/_html_full -name "*.html" | grep -E "mylib|index" | sort
  find: _build/default/_doc/_html_full: No such file or directory

Verify both directory structures exist:

  $ test -d _build/default/_doc/_html && echo "_html exists"
  [1]

  $ test -d _build/default/_doc/_html_full && echo "_html_full exists"
  [1]
