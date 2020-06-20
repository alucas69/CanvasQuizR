write_three_part_table= function(vs_header, vs_question, vs_answer=NULL,
                                 tablestyle="style=\"border:2px solid black\"",
                                 tableheaderstyle="style=\"border:2px solid black; background-color:#B1B1B1\"") {
  
  if (is.null(vs_answer)) output= NULL
  else {
    # answer part of table
    output= c(
      write_in_wrapper(write_in_wrapper("&nbsp;", "td"), "tr"),
      write_in_wrapper(write_in_wrapper(write_in_wrapper("Answers", "b"), "td"), "tr"),
      write_in_wrapper(
        write_in_wrapper(vs_answer, "td", block = TRUE), 
        "tr", block = TRUE
      )
    )
  }
  
  output= c(
    # heading part of table
    write_in_wrapper(
      write_in_wrapper(vs_header, "th", s_wrappertag = "style=\"text-align:left\"", block = TRUE),
      "tr", s_wrappertag = tableheaderstyle, block = TRUE
    ),
    # question part of table
    write_in_wrapper(
      write_in_wrapper(vs_question, "td", block = TRUE),
      "tr", block = TRUE
    ),
    output # answer part of table if present
  )
  output= write_in_wrapper(output, "table", s_wrappertag = tablestyle, block = TRUE)
}
