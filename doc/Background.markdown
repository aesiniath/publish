Background
==========

Web pages on are the global standard for displaying and searching
information but authoring content for them in raw HTML is tedious; this led
to the advent of lightweight markup formats like Markdown that could easily
be converted to HTML (it is no co-incidence that they represent documents
using formatting conventions prevalent in the early days of the internet
when users conveyed semantic information in text based mailing lists and
Usenet newsgroups using such lightweight markup).

Somewhat surprisingly, the web continues to struggle with taking content
into print form. Perhaps browsers are so overwhelmed by their own success;
certainly many people are happy to read content on screens surrounded by
flashy banner ads.

For certain audiences, however, getting high-quality printed output on
**paper** (or at least into a form that _could_ be printed to paper) is the
primary requirement. These users include researchers needing to document
results, business and organizations wishing to circulate content for review
and approval, writers wishing to produce their work in manuscript form
suitable for editing, and publishers needing to take content and actually
do pre-press rendering into a form "camera ready".

Images
------

Further complications arise when dealing with graphics. While LaTeX
grudgingly passes through raster images such as PNG and photos in JPEG form
(and results will be acceptable so long as the source image is of
sufficiently high resolution), the LaTeX typesetting toolchains have no
native support for SVG vector images.

This is a surprise to many users as SVG support has been dominant on the
web for some years and the target format, PDF, is itself a high-quality
vector format.

The solution, or at least work-around, is render (convert) the SVGs to a
PDFs first and then include these fragments in the typeset document. While
we tend to think of PDFs as "pages" it is at its essence just a way of
describing vector graphics, and (again not something you would have thought
of) you can include PDF fragments in \[what will become\] another PDF
document using the `\inclugegraphics` command.

See also:

 - [Getting Started](doc/Tutorial.markdown)
 - [Examples](doc/Examples.markdown)
