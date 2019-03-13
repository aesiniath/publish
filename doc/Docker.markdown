Docker Support
==============

Using Docker for LaTeX dependencies
-----------------------------------


Anyone who has used of LaTeX will be aware that rendering even a simple document
requires hundreds of packages to be installed. If you want to install the
packages yourself on your computer you can freely do so.

To help people get started we supply a built-in preamble; it still depends on
some 216 LaTeX packages, though. The process of working through trying to render a
document and one-by-one hunting down the packages you need to install can be
tedious.

So to compliment the built-in preamble we supply a prebuilt Docker image with
these packages already installed. You can instruct _render_ to run the LaTeX
processor in there, rather than on your own system, by specifying the `--docker`
option:

```shell
$ render --builtin-preamble --docker=oprdyn/publish-builtin:latest Trees.book
$
```

You are welcome to use any container you like. You need Latexmk installed (the
**latexmk** package) with the XeLaTeX processor installed (the
**texlive-xelatex** collection should pull it in) as _render_ will invoke
_latexmk_ command to build your resultant PDF. Images require that
_rsvg-convert_ is present (supplied by **librsvg2-tools** on Fedora).

If you specify the `--docker` option, _render_ will spawn a Docker container
from the image you specify, mount the temporary directory with the intermediate
fragments _render_ has generated into the container, and then run the necessary
_latexmk_ commands therein.

If you don't use the `--docker` option, _render_ runs the exact same commands,
but on your machine directly.

Other documentation:

 - [README](../README.markdown)
 - [Background](Background.markdown)
 - [Examples](Examples.markdown)

