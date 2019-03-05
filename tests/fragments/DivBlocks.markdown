(Note that this code tests attributes as well)

:::greeting
Hello world.

This is a test.
:::

::: {.greeting .earthlings}
Hello world

```
This is some code
```
:::

::: {#hello}
Hello world
:::

::: {#hello metadata=definite}
Hello world
:::

::: {.greeting .earthlings metadata=definite}
Hello world
:::

::: {#hello .greeting .earthlings metadata=definite}
Hello world
:::

Ultimately I'm not sure of the purpose of this syntax; it's rather HTML
centric in that it doesn't seem to do much good en-route to LaTeX.

