# only checks for stray "<" or ">"
simple_html_checker= function(vs_text) {
  text= paste(vs_text, collapse = " ")
  maxlen= stri_length(text)
  # count < and > and <...>
  position1= stri_locate_all(text, regex = ">")[[1]]
  number_gt= nrow(position1) - is.na(position1[1,1])
  position1= stri_locate_all(text, regex = "<")[[1]]
  number_lt= nrow(position1) - is.na(position1[1,1])
  position2= stri_locate_all(text, regex = "<[^<]+>")[[1]]
  number_tags= nrow(position2) - is.na(position2[1,1])
  # check for stray < or >
  if (number_lt != number_tags) stop("ERROR: STRAY < IN HTML")
  if (number_gt != number_tags) stop("ERROR: STRAY > IN HTML")
  if (number_tags == 0) return()
  # get html tags
  tag= rep(NA, number_tags)
  for (i1 in 1:number_tags) {
    position1= position2[i1,]
    # skip comments
    if (!(stri_sub(text, position1[1], min(maxlen, position1[1]+3)) == "<!--")) {
      # extract tag
      tag1= stri_sub(text, position1[1]+1, min(maxlen, position1[2]-1))
      position1= stri_locate(tag1, regex = "[^ ]+")
      tag[i1]= stri_sub(tag1, position1[1], position1[2])
    }
  }
  # check matching html tags
  simple_html_matching_tag_checker(tag[!is.na(tag)])
}


