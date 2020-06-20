write_as_html_table= function(m_text, vs_celltags=NULL, firstrowheader=FALSE,
                              tablestyle="style=\"border:2px solid black\"",
                              tableheaderstyle="style=\"border:2px solid black; background-color:#B1B1B1\"") {
  
  # initialize tags and check tags dimension
  if (!is.null(vs_celltags)) warning("Tags not implemented yet in write_as_html_table")
  
  # make table
  output= NULL
  for (i1 in 1:nrow(m_text)) {
    outputline= NULL
    wrapper= c("td", "th")[1 + (firstrowheader & (i1==1))]
    for (i2 in 1:ncol(m_text)) outputline= c(outputline, write_in_wrapper(m_text[i1,i2], wrapper))
    output= c(output, 
              write_in_wrapper(outputline, "tr", 
                               s_wrappertag = c("", tableheaderstyle)[1 + (firstrowheader & (i1==1))],
                               block = TRUE))
  }
  
  # return list
  return(write_in_wrapper(output, "table", s_wrappertag = tablestyle, block = TRUE))
}
