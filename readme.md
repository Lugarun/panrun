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
echo hello world
echo I miss babel
```

You can use the first line to determine the language mode for your editor.
You can use the second line for execution along the same rules as [panpipe](https://github.com/Warbo/panpipe).
