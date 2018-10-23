# Codemirror-mode-freemarker
This is a freemarker mode for Codemirror editor 2.x or later. This mode is based on htmlmixed mode, which adds highlighting and auto-indent for freemarker to Codemirror.

# how to use
Copy the `freemarker` directory to `~/codemirror/mode/`, and then indicate the editor mode as 'freemarker'.

Also you can add tag-folding support to freemarker by modify `~/codemirror/addon/fold/xml-fold.js`. Just add `#@` to the string variable `nameStartChar` , which makes `xmlTagStart` matchable to freemarker tag.  
