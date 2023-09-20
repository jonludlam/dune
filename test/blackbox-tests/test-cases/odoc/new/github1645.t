We create two libraries `l.one` and `l.two` with a conflicting module.
They build fine, are not co-linkable, but documentation should be able to be
built. See #1645.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > (package (name l))
  > EOF

  $ mkdir one
  $ cat > one/dune << EOF
  > (library
  >  (name l_one)
  >  (public_name l.one)
  >  (wrapped false))
  > EOF
  $ touch one/module.ml

  $ mkdir two
  $ cat > two/dune << EOF
  > (library
  >  (name l_two)
  >  (public_name l.two)
  >  (wrapped false))
  > EOF
  $ touch two/module.ml

  $ dune build @install
  $ dune build @doc-new
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.of_list_exn", { key = "default/_doc_new/html/docs/l/Module" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_rules__Odoc_new.has_rules in file
    "src/dune_rules/odoc_new.ml", line 2017, characters 4-78
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "vendor/fiber/src/scheduler.ml",
    line 76, characters 8-11
  -> required by ("gen-rules", In_build_dir "default/_doc_new/html/docs/l")
  -> required by ("load-dir", In_build_dir "default/_doc_new/html/docs/l")
  -> required by
     ("build-alias",
     { dir = In_build_dir "default/_doc_new/html/docs/l"; name = "doc-new" })
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias",
     { dir = In_build_dir "default/_doc_new/html/docs"; name = "doc-new" })
  -> required by ("<unnamed>", ())
  -> required by
     ("build-alias", { dir = In_build_dir "default"; name = "doc-new" })
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
