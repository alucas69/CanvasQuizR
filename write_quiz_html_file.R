write_quiz_html_file= function(s_filename, vs_text, s_dirname=".") {
  if (!dir.exists(s_dirname)) dir.create(s_dirname)
  f_quizfile <- file(sprintf("%s/%s", s_dirname, s_filename), "w")
  writeLines("<!DOCTYPE html>", f_quizfile)
  writeLines(
    write_in_wrapper(
      write_in_wrapper(vs_text, "body", block = TRUE),
      "html", s_wrappertag="xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\"", block = TRUE
    ), 
    f_quizfile
  )
  close(f_quizfile)
}

