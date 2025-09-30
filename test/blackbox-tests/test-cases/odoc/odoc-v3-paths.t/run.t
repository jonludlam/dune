Test that a specific odoc v3 file can be built directly

  $ dune build _build/default/_doc/_odoc/foo/foo/foo.odoc
  $ ls _build/default/_doc/_odoc/foo/foo/
  foo.odoc

Verify the odoc file was compiled with v3 flags (--parent-id and --output-dir)

  $ dune clean
  $ dune build _build/default/_doc/_odoc/foo/foo/foo.odoc --verbose 2>&1 | grep "odoc compile.*foo/foo.*foo.cmti"
  *odoc compile -I */.foo.objs/byte --output-dir * --parent-id foo/foo */.foo.objs/byte/foo.cmti (glob)