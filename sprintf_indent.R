# indent by i_indent * i_indentspace spaces
sprintf_indent= function(vs_text, i_indent=1, i_indentspace=3) {
  if ((i_indent >= 1) & (i_indentspace >= 1))
    vs_text= sprintf(sprintf("%%%ds%%s", as.integer(i_indent) * as.integer(i_indentspace)),
                     " ", vs_text)
  return(vs_text)
}
