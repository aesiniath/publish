Docker Support
==============

Using Docker for LaTeX dependencies
-----------------------------------

Anyone who has used of LaTeX will be aware that rendering even a simple document
requires hundreds of packages to be installed. If you want to install the
packages yourself on your computer you can freely do so.

To help people get started we supply an optional, builtin preamble; it still
depends on some 216 LaTeX packages, though. The process of working through
trying to render a document and one-by-one hunting down the packages you need
to install can be tedious.

So to compliment the builtin preamble we supply a prebuilt Docker image with
these packages already installed. You can instruct _render_ to run the LaTeX
processor in there, rather than on your own system, by specifying the
`--docker` option:

```shell
$ render --builtin-preamble --docker=oprdyn/publish-builtin:latest Trees.book
$
```

You are welcome to use any container you like. You need Latexmk installed (the
**latexmk** package) with the XeLaTeX processor installed (the
**texlive-xelatex** collection should pull it in) as _render_ will invoke
_latexmk_ command to build your resultant PDF. Images require that
_rsvg-convert_ is present (supplied by **librsvg2-tools** on Fedora) on your
host system.

If you specify the `--docker` option, _render_ will spawn a Docker container
from the image you specify, mount the temporary directory with the intermediate
fragments _render_ has generated into the container, and then run the necessary
_latexmk_ commands therein.

If you don't use the `--docker` option, _render_ runs the exact same commands,
but on your machine directly.

Docker Inception
----------------

You can also run the _render_ tool itself in a Docker container. There's an
image available at `docker.io/oprdyn/publish-render`. This means conceptually
you should be able to do:

```shell
$ docker run \
    --rm=true \
    --volume=`pwd`:/mnt \
    oprdyn/publish-render:latest \
        render \
            --builtin-preamble \
            --docker=oprdyn/publish-builtin:latest \
            Trees.book
$
```

Nothing is ever simple in Dockerland, however. The first problem is that the
_docker_ command line program needs to be installed in the container that
_render_ is running in. When you just run these programs ordinarily on a Linux
host then it of course has access to run Docker. But if run inside a container
we need to install the binary and make the host's "docker control socket"
available to it:

```shell
$ docker run \
    --rm=true \
    --volume=/var/run/docker.sock:/var/run/docker.sock \
    --volume=`pwd`:/mnt \
    mypublish:latest \
        render \
            --builtin-preamble \
            --docker=oprdyn/publish-builtin:latest \
            Trees.book
$
```

where `mypublish` is a locally created image built from `oprdyn/publish-render`
that adds the **docker-ce-cli** and **librsvg2-bin** packages.

The second trouble is that there's no way to get the temporary directory
(normally created with a random name by _render_ in _/tmp/publish-XXXXXX_ and
recorded in _.target_) that is in the outer container that _render_ is running
in mounted into the inner container that the _latexmk_ process runs in.

You could get this to work if you "volume mount" the temporary directory in,
but you have to do it from the **host**, because that's where the docker engine
is; volumes requested from within one container (the outer one) won't be in the
same namespace and thus will appear empty in the second (inner) container.

We added an option to _render_ allowing you to override the temporary directory
and manually force the directory name to be used. Creating it on the host,
volume mounting it in to the outer container and then using `--temp` to specify
it to the inner one works:

```shell
$ mkdir /tmp/publish-local
$ docker run \
    --rm=true \
    --volume=/var/run/docker.sock:/var/run/docker.sock \
    --volume=/tmp/publish-local:/tmp/publish-local \
    --volume=`pwd`:/mnt \
    mypublish:latest \
        render \
            --temp=/tmp/publish-local \
            --builtin-preamble \
            --docker=oprdyn/publish-builtin:latest \
            Trees.book
$
```

This could probably be easier, but it is at least possible, and how our users
on Mac OS X are able to use the **publish** tools.

The usual caveats about how evil it is to mount the Docker socket into a
container apply. Don't do this at home. Or in prod at work, come to think of
it.

Other documentation:

 - [README](../README.markdown)
 - [Background](Background.markdown)
 - [Examples](Examples.markdown)

