html_escape <- function(vs_text, convert_ampersand=TRUE) {
  for (i1 in 1:length(vs_text)) {
    if (convert_ampersand) vs_text[i1] <- stri_replace_all(vs_text[i1], "&amp;", regex = "&")
    vs_text[i1] <- stri_replace_all(vs_text[i1], "&lt;", regex = "<")
    vs_text[i1] <- stri_replace_all(vs_text[i1], "&gt;", regex = ">")
  }
  return(vs_text)
}



