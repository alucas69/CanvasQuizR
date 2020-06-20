write_as_html_ul= function(vs_text, vs_tags=NULL) {
  # initialize tags and check tags dimension
  if (is.null(vs_tags)) vs_tags= rep("", length(vs_text))
  else if (length(vs_tags) == length(vs_text)) vs_tags= sprintf(" %s", vs_tags)
  else stop(sprintf("Wrong length of tags (%d) versus text (%d) in write_as_html_ul", length(vs_tags), length(vs_text)))
  
  # make list entries
  output= write_in_wrapper(vs_text, "li", vs_tags)
  
  # return list
  return( write_in_wrapper(output, "ul", block = TRUE) )
}
