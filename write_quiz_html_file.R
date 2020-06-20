write_quiz_html_file= function(s_filename, vs_text, s_dirname=".") {
  if (!dir.exists(s_dirname)) dir.create(s_dirname)
  f_quizfile <- file(sprintf("%s/%s", s_dirname, s_filename), "w")
  writeLines(
    write_in_wrapper(
      write_in_wrapper(vs_text, "body", block = TRUE),
      "html", block = TRUE
    ), 
    f_quizfile
  )
  close(f_quizfile)
}

