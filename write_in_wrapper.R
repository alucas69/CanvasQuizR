# write each row of vector of strings vs_text into html wrapper s_wrappername with s_wrappertag 
# if block==FALSE. Do so inline if inline=TRUE to get a more condensed html file.
# if block == TRUE, write the whole vector of strings into the wrapper (rather than each
# element separately)
write_in_wrapper= function(vs_text, s_wrappername, s_wrappertag="", inline=TRUE, block=FALSE) {
  if (stri_length(s_wrappertag) > 0) s_wrappertag= paste(" ", s_wrappertag, sep="")
  if (block) return(c(
    sprintf("<%s%s>", s_wrappername, s_wrappertag),
    sprintf_indent(vs_text),
    sprintf("</%s>", s_wrappername)
  )) else {
    if (inline) return(sprintf("<%s%s>%s</%s>", s_wrappername, s_wrappertag, vs_text, s_wrappername))
    else return(rbind(
      sprintf("<%s%s>", s_wrappername, s_wrappertag),
      rbind(sprintf_indent(vs_text), sprintf("</%s>", s_wrappername))
    ))
  }
}


