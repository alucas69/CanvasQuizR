# write colored true/false ul list
write_as_html_ul_color= function(vs_text, vb_tags, truecolor="green", falsecolor="red") {
  if (length(vs_text) != length(vb_tags)) stop("tags not same length as text in write_as_html_ul_color")
  if (!is.logical(vb_tags)) stop("tags does not hold booleans for indicating true/false")
  vs_tags= sprintf("style=\"color:%s; list-style-type:%s\"", 
                   c(falsecolor, truecolor)[1+as.numeric(vb_tags)],
                   c("circle", "disc")[1+as.numeric(vb_tags)])
  return( write_as_html_ul(vs_text, vs_tags) )
}
