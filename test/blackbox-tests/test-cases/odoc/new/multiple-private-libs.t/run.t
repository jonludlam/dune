This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-new 2>&1 | grep test
        ocamlc a/.test.objs/byte/test.{cmi,cmo,cmt}
        ocamlc b/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc _doc_new/index/private/test@ea8c79305c05/page-test@ea8c79305c05.odoc
          odoc _doc_new/index/private/test@6aabb9861046/page-test@6aabb9861046.odoc
          odoc _doc_new/odoc/internal/test@6aabb9861046/test.deps
          odoc _doc_new/odoc/internal/test@ea8c79305c05/test.deps
          odoc _doc_new/odoc/internal/test@6aabb9861046/test.odoc
          odoc _doc_new/odoc/internal/test@ea8c79305c05/test.odoc
          odoc _doc_new/odoc/internal/test@6aabb9861046/test.odocl
          odoc _doc_new/index/private/test@6aabb9861046/page-test@6aabb9861046.odocl
          odoc _doc_new/odoc/internal/test@ea8c79305c05/test.odocl
          odoc _doc_new/index/private/test@ea8c79305c05/page-test@ea8c79305c05.odocl
          odoc _doc_new/html/docs/test@6aabb9861046/Test
          odoc _doc_new/html/docs/test@6aabb9861046/index.html
          odoc _doc_new/html/docs/test@ea8c79305c05/Test
          odoc _doc_new/html/docs/test@ea8c79305c05/index.html
