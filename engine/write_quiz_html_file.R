write_quiz_html_file= function(s_filename, vs_text, s_dirname=".") {
  if (!dir.exists(s_dirname)) dir.create(s_dirname)
  f_quizfile <- file(sprintf("%s/%s", s_dirname, s_filename), "w")
  # identify as html
  writeLines("<!DOCTYPE html>", f_quizfile)
  # head part
  output= write_in_wrapper(
    c(
      # font choices
      write_in_wrapper(
        c(
          "h1, p, b, tr, td, ul, li {font-family:Calibri, Tahoma, verdana, cursive, sans-serif;}",
          "table {width:100%; max-width:18cm}"
        ),
        "style", block=TRUE
      ),
      # mathml renderer (script command)
      write_in_wrapper("", "script", s_wrappertag = "type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML\"")
    ), "head", block=TRUE
  )
  # add body part
  vs_text= c(output, write_in_wrapper(vs_text, "body", block = TRUE))
  # wrap in html
  vs_text= write_in_wrapper(vs_text, "html", s_wrappertag="xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\"", block = TRUE)
  # write and close
  writeLines(vs_text, f_quizfile)
  close(f_quizfile)
}

