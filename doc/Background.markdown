Background
==========

Web pages are the global standard for displaying and searching information
but authoring content for them in raw HTML is tedious. This led to the
advent of lightweight markup formats like Markdown that could easily be
converted to HTML (it is no co-incidence that these styles represent
documents using formatting conventions that were evolved in the early days
of the internet by users who wanted to conveyed semantic information in
text-based mailing lists and Usenet newsgroups).

Somewhat surprisingly, the web continues to struggle with taking content
into print form. Perhaps browser vendors are so overwhelmed by their own
success that they don't feel the need to cater for this use case; certainly
many people are happy to read content on screens surrounded by flashy
banner ads.

For certain audiences, however, getting high-quality printed output on
**paper** (or at least into a form that _could_ be printed to paper) is the
primary requirement. These users include

 - researchers needing to document results;
 - students submitting essays and other papers;
 - engineers writing requirements, design, and system documentation;
 - business and organizations wishing to circulate content for review and
   approval;
 - authors wishing to produce their work in manuscript form suitable for
   editing;
 - publishers needing to do typesetting and actual pre-press rendering of
   manuscripts into "camera ready" form; and
 - humans who don't like flashy banner ads.

So we want to work in Markdown, but render to PDF. The challenges and
complications of this process are considerable. Fortunately there is an
awesome tool that can help: Pandoc.

Pandoc is a document conversion tool. It has a wide variety of "readers"
which take as input any of number of different document formats and
converts them to an internal representation which is then suitable for any
one of various "writers" to onwards convert them to the desired target
format.

After considerable usage (which is to say, fighting with) the _pandoc_
command and the "templates" it ships with we had learned enough to realize
we didn't need it to render the PDF but could instead rely on it to get us
to LaTeX as an intermediate format. Our solution was to use the _pandoc_
command to convert _.markdown_ files to _.latex_ and then invoke _pdflatex_
ourselves to get the desired _.pdf_ output. We later switched to _latexmk_
to handle the multiple passes necessary to resolve cross-references arising
when rendering a LaTeX document.

Pandoc is itself a (very large) Haskell library, so it was not a
particularly earthshattering conceptual leap to consider calling into the
library directly from a wrapper program ourselves, especially as we were no
longer relying on it to build the PDF for us.

**publish**, then, is a tool which allows you to specify the files
comprising a manuscript, converts them from Markdown to LaTeX, then
combines them together as input to the LaTeX processor for conversion to
Portable Document Format ready for previewing or printing.

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

Further reading:

 - [Getting Started](doc/Tutorial.markdown)
 - [Examples](doc/Examples.markdown)
