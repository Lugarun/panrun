# Introduction

I hear you miss org-mode.
This script tries to help markdown patch that hole by providing functionality for your favourite text editor to open a markdown code block in a seperate buffer with langauge features enabled.
Furthermore, if you use the panpipe syntax for code block execution, you are able to execute code blocks as well.

Note that as of now you have to integrate this with your editor yourself.
All that this script does is use pandoc to parse (in two ugly passes) your markdown file and return the info for the codeblock at the given line number.
