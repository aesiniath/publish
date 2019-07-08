Tutorial
========

Let's say we want to write a book about trees.

You start with writing your content in a text file using Markdown syntax to
add semantic markup to the text as you see fit. We'll make the assumption
that you know the basics of Markdown syntax from having used it on GitHub,
your blog, or elsewhere. If you need to learn more about Markdown syntax,
see this [tutorial](https://commonmark.org/help/)).

For our example,

```text
On the subject of trees
=======================

This may come as a complete surprise
to you, but trees are **green**.

```

Put your text into a file called _Introduction.markdown_.

You now need to tell **publish** which files make up the document you want
to render. Create another file which lists the pieces of your manuscript,
one per line. Here we've only got one fragment, so this won't take long:

```text
% publish v2
% begin
Introduction.markdown
% end
```

Put the list into a file named _Trees.book_. The filename extension does
not matter, but we've adopted the convention of using _.book_ to identify
such "bookfiles". The basename of the file _does_ matter; it will be used
to name the PDF we're going to generate.

Now you can render your document. The tool installed by **publish** package
is called _render_. Run that as follows:

```shell
$ render -p Trees.book
$
```

That's it! If you want a bit more detail about what it's doing, you can use
`--verbose` (or `-v` for short):

```shell
$ render -p -v Trees.book
08:52:24Z (00000.002) Reading bookfile
08:52:24Z (00000.003) Setup temporary directory
08:52:24Z (00000.004) Convert document fragments to LaTeX
08:52:24Z (00000.004) Write intermediate LaTeX file
08:52:24Z (00000.004) Render document to PDF
08:52:24Z (00000.085) Copy resultant document here
08:52:24Z (00000.087) Complete
$
```

either way you've now got a file called _Trees.pdf_:

```
$ ls
Introduction.markdown
Trees.book
Trees.pdf
$
```

Open that with your favourite PDF viewer and you'll see your fabulous book
about the arboreal arts, ready to be sent to the printer.

Preamble
--------

The `-p` in the above example was important. It's short for
`--builtin-preamble`. Using that option tells the _render_ program to wrap
a simple built-in LaTeX preamble around your document. 

More advanced users will happily use their own LaTeX preamble based on
years of experience writing academic papers or typesetting mathematical
memoirs. They should put their preamble as the first item in the bookfile,
perhaps:

```
% publish v2
preamble.latex
% begin
Introduction.markdown
% end
```

Docker integration
------------------

Assuming you _don't_ have years of experience using LaTeX toolchains, you can
use a presupplied built-in preamble. If you want to install the packages
yourself you can freely do so. There is also an option to run the render in a
Docker container.

```shell
$ render --builtin-preamble --docker=oprdyn/publish-builtin:latest Trees.book
$
```

If you specify the `--docker` option _render_ will spawn a Docker container
from the image you specify. The image shown above has the dependencies You're
welcome to use any container you like. Further details are on the
[Docker](Docker.markdown) page.

See also:

 - [Further examples](Examples.markdown)
 - [Background](Background.markdown)
