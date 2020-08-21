Examples
========

Subdirectories
--------------

A work with multiple chapters and images in different subdirectories could be
described as follows

```
% publish v2
preamble.tex
% begin
chapters/Introduction.md
chapters/RelatedWork.md
chapters/Results.tex
chapters/chart-07.svg
chapters/Analysis.md
chapters/Conclusion.md
generated/References.tex
% end
```

If you put that list into _EnormousThesis.book_ in the current directory it
can be rendered as follows:

```
$ render EnormousThesis.book
```

the result will be written to _EnormousThesis.pdf_, assuming you had
`\documentclass` in _preamble.tex_, along with all the prerequisite LaTeX
packages installed on your system.

Including images
----------------

In the file _chapters/Analysis.md_ the markup used to include the SVG
image would be:

```markdown
![A plot showing our analysis](chart-07.pdf)
```

Note that the filename extension is _.pdf_ not _.svg_. **publish** will
convert the SVG to a PDF fragment suitable for inclusion in your output
document, so you need to tell the LaTeX processor to include that, not the
source SVG.

Of course this translates to a LaTeX command,

```latex
\includegraphics{chart-07.pdf}
```

which you can use inline in _.md_ files or raw in _.tex_ source files.

Other documentation:

-   [Getting Started](Tutorial.md)
-   [Background](Background.md)
-   [README](../README.md)
