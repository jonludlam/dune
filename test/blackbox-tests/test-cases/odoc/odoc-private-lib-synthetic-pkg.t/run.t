Test that private libraries (without public_name) get synthetic package names
based on lib_unique_name (which includes a hash).

Build documentation for private libraries:

  $ dune build @doc-private

Check that odoc files are generated with the synthetic package name:

  $ find _build/default/_doc/_odoc -name 'privatelib.odoc' | head -1
  _build/default/_doc/_odoc/privatelib@e4ac9fdbbbe6/privatelib.odoc

Verify the path structure uses the synthetic package name (lib@hash):

  $ ls -d _build/default/_doc/_odoc/privatelib@*
  _build/default/_doc/_odoc/privatelib@e4ac9fdbbbe6

Check that odocl files use the same synthetic package structure:

  $ find _build/default/_doc/_odocls -name 'privatelib.odocl' | head -1
  _build/default/_doc/_odocls/privatelib@e4ac9fdbbbe6/privatelib.odocl

Verify the synthetic package name is stable (same hash on rebuild):

  $ HASH1=$(ls -d _build/default/_doc/_odoc/privatelib@* | sed 's/.*@//')
  $ dune clean
  $ dune build @doc-private
  $ HASH2=$(ls -d _build/default/_doc/_odoc/privatelib@* | sed 's/.*@//')
  $ test "$HASH1" = "$HASH2" && echo "Hash is stable"
  Hash is stable
