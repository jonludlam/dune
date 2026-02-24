.. _documentation:

************************
Generating Documentation
************************

.. TODO(diataxis)

   Split between:

   - A "generating API documentation" how-to guide
   - Some reference documentation

Prerequisites
=============

Documentation in Dune is done courtesy of the odoc_ tool. Therefore, to
generate documentation in Dune, you will need to install this tool. This
should be done with opam:

::

  $ opam install odoc

Writing Documentation
=====================

Documentation comments will be automatically extracted from your OCaml source
files following the syntax described in the section ``Text formatting`` of
the `OCaml manual <http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html>`_.

Additional documentation pages may be attached to a package using the
:doc:`/reference/dune/documentation` stanza.

Building Documentation
======================

To generate documentation using the :doc:`/reference/aliases/doc` alias, all
that's required to is to build this alias:

.. code:: console

  $ dune build @doc

An index page containing links to all the opam packages in your project can be
found in:

.. code:: console

  $ open _build/default/_doc/_html/index.html

Documentation for private libraries may also be built with
:doc:`/reference/aliases/doc-private`:

.. code:: console

  $ dune build @doc-private

But these libraries will not be in the main HTML listing above, since they
don't belong to any particular package, but the generated HTML will still be
found in ``_build/default/_doc/_html/<library>``.


Documentation Dependencies
==========================

When building documentation, odoc needs to know which packages and libraries
are available for cross-referencing. There are three sources of documentation
dependencies, each controlling what you can reference in different contexts:

Library Dependencies (``-L`` flags)
------------------------------------

Every library listed in a ``(libraries ...)`` stanza, plus its full transitive
closure of dependencies, is available for cross-referencing in that library's
``.ml`` and ``.mli`` files. This means if your library depends on
``odoc-parser``, you can write ``{!Odoc_parser.t}`` in your doc comments and
it will resolve.

These dependencies are determined automatically from your ``(libraries ...)``
stanza and require no extra configuration.

Package Dependencies (``-P`` flags)
------------------------------------

Package-level flags tell odoc where to find compiled documentation for entire
packages. These are derived from two sources:

1. **Explicit ``:with-doc`` dependencies**: Packages listed with ``:with-doc``
   in your ``(depends ...)`` stanza. For example:

   .. code-block:: dune

      (package
       (name mypackage)
       (depends (odoc :with-doc)))

   This tells Dune that ``odoc`` is a documentation dependency. When your
   package is installed, the generated ``odoc-config.sexp`` file will list
   ``odoc`` so that downstream consumers know about the relationship.

2. **Transitive library dependencies**: Any package that contains a library in
   the transitive closure of your library dependencies. For example, if your
   library depends on ``odoc-parser`` (which belongs to the ``odoc-parser``
   package), that package automatically gets ``-P`` flags even if it is not
   listed as a ``:with-doc`` dependency.

What ``:with-doc`` Is For
--------------------------

The ``:with-doc`` filter serves two purposes:

- **Documentation-only dependencies**: If you want to reference a package in
  your documentation pages (``.mld`` files) but don't depend on it as a
  library, use ``:with-doc``. For example, you might reference ``cmdliner``
  types in doc comments without actually depending on the ``cmdliner`` library:

  .. code-block:: dune

     (package
      (name mypackage)
      (depends (cmdliner :with-doc)))

- **Installed package metadata**: When your package is installed, ``:with-doc``
  dependencies are recorded in ``odoc-config.sexp``. This allows tools
  building documentation for installed packages to discover the full set of
  packages needed.

You do **not** need to add ``:with-doc`` for packages that your libraries
already depend on. If ``mylib`` has ``(libraries odoc-parser)`` in its
``dune`` file, ``odoc-parser`` will automatically be available for
cross-referencing without any ``:with-doc`` entry.

Summary
-------

+-----------------------------------+-------------------------------------------+
| What you want to reference        | What you need                             |
+===================================+===========================================+
| Types/values from a library your  | Nothing extra — resolved automatically    |
| code depends on (in ``.ml`` /     | from the ``(libraries ...)`` stanza.      |
| ``.mli`` doc comments)            |                                           |
+-----------------------------------+-------------------------------------------+
| Types/values from a library your  | Add ``(dep :with-doc)`` to your package's |
| code does **not** depend on (in   | ``(depends ...)`` stanza.                 |
| ``.mld`` pages or doc comments)   |                                           |
+-----------------------------------+-------------------------------------------+
| All local packages in your        | Nothing extra — all local packages are    |
| workspace                         | included automatically.                   |
+-----------------------------------+-------------------------------------------+
| Installed packages in ``@doc``    | Automatically remapped to ocaml.org URLs. |
| mode                              | No configuration needed.                  |
+-----------------------------------+-------------------------------------------+


Documentation Stanza: Examples
------------------------------

The :doc:`/reference/dune/documentation` stanza will attach all the
``.mld`` files in the current directory in a project with a single package.

.. code-block:: dune

   (documentation)

This stanza will attach three ``.mld`` files to package ``foo``. The ``.mld`` files should
be named ``foo.mld``, ``bar.mld``, and ``baz.mld``

.. code-block:: dune

   (documentation
    (package foo)
     (mld_files foo bar baz))

This stanza will attach all ``.mld`` files to the inferred package, 
excluding ``wip.mld``, in the current directory:

.. code-block:: dune

   (documentation
    (mld_files :standard \ wip))

All ``.mld`` files attached to a package will be included in the generated
``.install`` file for that package. They'll be installed by opam.

.. code-block:: dune

   (documentation
    (files
     (glob_files_rec
      (doc/* with_prefix .))))

All files in the ``doc/`` folder will be attached to the inferred package. The
hierarchy between them will be preserved, relative to ``doc/`` considered as the
root.

.. note::

   ``dune`` does not yet support building the documentation with a non-flat
   hierarchy, or with non-mld files. However, it supports installing those files
   following a convention, so that ``odoc_driver`` can build the docs with
   hierarchy and asset files.


Package Entry Page
------------------

The ``index.mld`` file (specified as ``index`` in ``mld_files``) is treated
specially by Dune. This will be the file used to generate the entry page for
the package, linked from the main package listing.

To generate pleasant documentation, we recommend writing an ``index.mld`` file
with at least short description of your package and possibly some examples.

If you do not write your own ``index.mld`` file, Dune will generate one with
the entry modules for your package. But this generated file will not be
installed.

.. _odoc-options:

Passing Options to ``odoc``
===========================

.. code-block:: dune

    (env
     (<profile>
      (odoc <optional-fields>)))

See :doc:`/reference/dune/env` for more details on the ``(env ...)``
stanza. ``<optional-fields>`` are:

- ``(warnings <mode>)`` specifies how warnings should be handled. ``<mode>``
  can be: ``fatal`` or ``nonfatal``. The default value is ``nonfatal``. This
  field is available since Dune 2.4.0 and requires odoc_ 1.5.0.

.. _odoc: https://github.com/ocaml-doc/odoc

Local Documentation Search Using Sherlodoc
==========================================

If Sherlodoc is installed, generated HTML documentation will include a
search bar. It supports search by name, documentation and fuzzy type search.

In can be installed with:

.. code:: console

  $ opam install sherlodoc
