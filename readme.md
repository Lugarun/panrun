# Introduction

I hear you miss [org-mode](https://orgmode.org/).
This script tries to help markdown patch that hole in your heart by providing functionality for your favourite text editor to open a markdown code block in a separate buffer with language features enabled.
Furthermore, if you use the [panpipe](https://github.com/Warbo/panpipe) syntax for code block execution, you can execute code blocks as well.

Note that as of now you have to integrate this with your editor yourself.
All that this script does is use [pandoc](https://pandoc.org/) to parse (in two ugly passes) your markdown file and return the info for the code block at the given line number.

![demo](https://github.com/Lugarun/panrun/assets/5767106/55e6c223-ad72-4dd3-8c00-ac7f3d7798ea)

# Example

If we have this code block example:

```{.sh pipe="bash"}
echo hello world
echo I miss babel
```

Then we can run `nix run .#panrun -- -n 18 readme.md` to get the following:

```
sh
bash
16 20
echo hello world
echo I miss babel
```

You can use the first line to determine the language mode for your editor.
You can use the second line for execution along the same rules as [panpipe](https://github.com/Warbo/panpipe).
Finally you can use the third line to identify which codeblock exactly was used, this is useful for inserting the results of a codeblock edit back into the markdown file (assuming it hasn't changed).

# Kakoune

Here is how you might configure kakoune to use panrun.

```
declare-option str babel_pipe_class "pipe" # this option allows you to change the code block attribute name that holds the piping information

define-command -override -docstring "orgmode style code buffer" babel-buffer %{
  eval %sh{
    echo write
    output=$(mktemp -d -t kak-temp-XXXXXXX)
    panrun -f ${kak_buffile} -n ${kak_cursor_line} -p ${kak_opt_babel_pipe_class} > ${output}/panrun
    if [ $? -eq 0 ]; then
      tail -n+4 ${output}/panrun > ${output}/file
      head -n 1 ${output}/panrun > ${output}/filetype
      head -n 2 ${output}/panrun | tail -1 > ${output}/run
      head -n 3 ${output}/panrun | tail -1 > ${output}/position
      ln -s $(pwd) ${output}/root

      echo "edit! ${output}/file"
      echo set buffer filetype $(cat ${output}/filetype)
      echo "hook buffer BufClose .* %{ nop %sh{ sed -e '$(cat ${output}/position | awk '{print $1+=1}')r ${output}/file' -e $(cat ${output}/position | awk '{print $1+=1}'),$(cat ${output}/position | awk '{print $2-=2}')d -i ${kak_buffile}; rm -r $output};}"
      echo map buffer user e "':babel-execute-buffer $output <ret>'" "-docstring 'execute babel buffer'"
    else
      echo "echo Panrun failed"
    fi
  }
}

define-command -override -docstring "execute babel buffer" -params 1 babel-execute-buffer %{
  kakpipe -- sh -c "pushd '%arg{1}' > /dev/null; cat file | bash run; popd > /dev/null"
}
```
