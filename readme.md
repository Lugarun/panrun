# Introduction

I hear you miss [org-mode](https://orgmode.org/).
This script tries to help markdown patch that hole in your heart by providing functionality for your favourite text editor to open a markdown code block in a separate buffer with language features enabled.
Furthermore, if you use the [panpipe](https://github.com/Warbo/panpipe) syntax for code block execution, you can execute code blocks as well.

Note that as of now you have to integrate this with your editor yourself.
All that this script does is use [pandoc](https://pandoc.org/) to parse (in two ugly passes) your markdown file and return the info for the code block at the given line number.

# Example

If we have this code block example:

```{.sh pipe="bash"}
echo hello world
echo I miss babel
```

Then we can run `nix run .#panrun -- -f readme.md -n 14` to get the following:

```
sh
bash
14 18
echo hello world
echo I miss babel
```

You can use the first line to determine the language mode for your editor.
You can use the second line for execution along the same rules as [panpipe](https://github.com/Warbo/panpipe).
Finally you can use the third line to identify which codeblock exactly was used, this is useful for inserting the results of a codeblock edit back into the markdown file (assuming it hasn't changed).

# Kakoune

Here is how you might configure kakoune to use panrun.

```
define-command -docstring "orgmode style code buffer" babel %{
  eval %sh{
    output=$(mktemp -d -t kak-temp-XXXXXXX)/file
    panrun -f ${kak_buffile} -n ${kak_cursor_line} | tail -n+4 > ${output}

    echo "edit! ${output}" 
    echo set buffer filetype $(panrun -f ${kak_buffile} -n ${kak_cursor_line} | head -n 1)
  }
}
```
