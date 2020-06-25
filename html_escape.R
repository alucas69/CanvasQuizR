html_escape= function(vs_text, convert_ampersand=TRUE, removenonascii=TRUE) {
  if (convert_ampersand) vs_text[i1]= stri_replace_all(vs_text[i1], "&amp;", regex = "&")
  vs_text= stri_replace_all(vs_text, "&lt;", regex = "<")
  vs_text= stri_replace_all(vs_text, "&gt;", regex = ">")
  if (removenonascii) vs_text= iconv(vs_text, "ascii")
  return(vs_text)
}



