Test that a custom workspace-level index.mld can be specified in dune-project.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (name mylib)
  > (doc_index docs/index.mld)
  > (package (name mylib))
  > EOF

  $ mkdir -p docs
  $ cat > docs/index.mld << EOF
  > {0 My Custom Documentation Index}
  > 
  > This is a custom index page for the workspace.
  > 
  > {1 Packages}
  > 
  > - {{!mylib}mylib} - My library
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml << EOF
  > (** My library module *)
  > let hello () = "Hello"
  > EOF

Build the documentation:

  $ dune build @doc 2>&1

Verify the custom index content is used (check the mld file in the build):

  $ cat _build/default/_doc/_index/index.mld
  {0 My Custom Documentation Index}
  
  This is a custom index page for the workspace.
  
  {1 Packages}
  
  - {{!mylib}mylib} - My library







Verify the HTML is generated:

  $ test -f _build/default/_doc/_html/index.html && echo "index.html exists"
  index.html exists
