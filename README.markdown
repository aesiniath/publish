Publishing tools for papers, books, and presentations
=====================================================

Authoring a high quality document and rendering it to a PDF suitable for
printing means using a toolchain different than those used to assemble
content into web pages. LaTeX processors are the dominant way to typeset
documents, but while the results are often beautiful it is an environment
which has accreted cruft over decades and can be difficult to use. We have
faith that the gods in the computing pantheon will someday grant us peace
(e.g. the end of the editor wars) and better tools (or at least ones that
give useful error messages), but while we wait for the millennium (when
LaTeX will be replaced with something whose syntax doesn't make your eyes
bleed) we wanted to see if we could offer a slightly less unpleasant
authoring experience.

Ideally we could:

  - write content in lightly marked-up plain text (_i.e._ Markdown) as much as
    possible;

  - use localized typesetting commands inline when slightly greater
    expressiveness is needed; and

  - directly pass-through entire blobs of LaTeX when complex incantations and
    arcane summonings are necessary to appease the vengeful daemons who
    make the office printer work.

and that is what **publish** provides. It gives you a way to:

 1. list the _.markdown_ and _.latex_ files that make up your document;

 2. convert Markdown fragments into LaTeX using **pandoc**;

 3. use **librsvg** to convert _.svg_ images into something that can be
    included by the LaTeX processors; and

 4. combine the resultant intermediate pieces and render them to a _.pdf_
    using the **xetex** toolchain.

Documentation
-------------

There is a [getting started][Tutorial] tutorial, [background][Background]
notes, and several [examples][Examples]. And you can get help from the
command-line:

```
$ render --help
```

[Tutorial]: doc/Tutorial.markdown
[Background]: doc/Background.markdown
[Examples]: doc/Examples.markdown
