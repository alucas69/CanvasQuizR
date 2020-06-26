# interpret names between [...] as variable names and highlight them in the 
# text; also return the names.
write_quiz_html_mb_highlight_variables= function(vs_text, s_highlightwrapper="strong", 
                                                 s_highlighttag="style=\"color:blue; font-family: Courier new\"") {
  output= list(text=vs_text, variables=NULL)
  # loop over the text elements
  for (i0 in 1:length(vs_text)) {
    s_text= vs_text[i0]
    # locate canvas variables [...]
    positions = stri_locate_all(s_text, regex = "\\[[a-zA-Z1-9!\\]]+\\]")[[1]]
    if (!is.na(positions[1,1])) { # if there are variables, process
      if (positions[1,1] > 1) output$text[i0]= stri_sub(s_text, 1, positions[1,1]-1)
      else output$text[i0]= ""
      for (i1 in 1:nrow(positions)) {
        # store variable if new
        mb_variable= stri_sub(s_text, positions[i1,1], positions[i1,2]) 
        if (!(mb_variable %in% output$variables)) output$variables= c(output$variables, mb_variable)
        # highlight variable name
        output$text[i0]= paste(output$text[i0], write_in_wrapper(mb_variable, s_highlightwrapper, s_wrappertag = s_highlighttag), sep="")
        # past intermediate (or final) text piece
        if (i1 == nrow(positions)) i2= stri_length(s_text)
        else i2= positions[i1+1,1] - 1
        output$text[i0]= paste(output$text[i0], stri_sub(s_text, positions[i1,2]+1, i2), sep="")
      }
    }
  }
  return(output)
}
