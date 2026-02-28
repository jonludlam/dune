Basic markdown generation with a single library:

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > (package
  >  (name l))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name l))
  > EOF

  $ cat > l.ml << EOF
  > module M = struct
  >   type t = int
  > end
  > EOF

  $ list_md () {
  >   find _build/default/_doc/_markdown -name '*.md' 2>/dev/null | sort
  > }

  $ list_html () {
  >   find _build/default/_doc/_html -name '*.html' 2>/dev/null | sort
  > }

  $ dune build @doc-md
  $ list_md
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/l/index.md
  _build/default/_doc/_markdown/l/l/L-M.md
  _build/default/_doc/_markdown/l/l/L.md
  _build/default/_doc/_markdown/l/l/index.md

@doc-md does not generate HTML files:

  $ list_html

@doc will continue generating doc as usual (but no markdown):

  $ dune build @doc
  $ list_html
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/l/index.html
  _build/default/_doc/_html/l/l/L/M/index.html
  _build/default/_doc/_html/l/l/L/index.html
  _build/default/_doc/_html/l/l/index.html

Multiple libraries in a package, with a custom page:

  $ dune clean
  $ rm -f l.ml l.opam dune

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package
  >  (name mypkg))
  > EOF

  $ mkdir -p lib1 lib2 doc/mypkg

  $ cat > lib1/dune << EOF
  > (library
  >  (name lib1)
  >  (public_name mypkg.lib1))
  > EOF

  $ cat > lib1/foo.ml << EOF
  > type t = int
  > let x = 42
  > EOF

  $ cat > lib1/bar.ml << EOF
  > type t = string
  > let y = "hello"
  > EOF

  $ cat > lib2/dune << EOF
  > (library
  >  (name lib2)
  >  (public_name mypkg.lib2))
  > EOF

  $ cat > lib2/baz.ml << EOF
  > type t = bool
  > let z = true
  > EOF

  $ cat > doc/mypkg/index.mld << EOF
  > {0 My Package}
  > Welcome to mypkg.
  > EOF

Each library gets its own flat directory with all modules:

  $ dune build @doc-md
  $ list_md
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/mypkg/index.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1-Bar.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1-Foo.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/index.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/Lib2-Baz.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/Lib2.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/index.md

Verify the flat structure - no per-module subdirectories:

  $ find _build/default/_doc/_markdown/mypkg -type d | sort
  _build/default/_doc/_markdown/mypkg
  _build/default/_doc/_markdown/mypkg/mypkg.lib1
  _build/default/_doc/_markdown/mypkg/mypkg.lib2

  $ find _build/default/_doc/_markdown/mypkg -type f | sort
  _build/default/_doc/_markdown/mypkg/index.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1-Bar.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1-Foo.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/Lib1.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib1/index.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/Lib2-Baz.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/Lib2.md
  _build/default/_doc/_markdown/mypkg/mypkg.lib2/index.md
