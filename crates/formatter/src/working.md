Options {
    line_width : Integer


}



group -> Keeps breaking until the content is less than or equal to the line width. If any content inside group breaks, the whole group breaks. Outermost group breaks first when printers width is exceeded.





Array, indent, dedent ..., except for group -> Breaks in array will break only if printers width is exceeded, except for hardline.


working

expand join cmd
propagate break
