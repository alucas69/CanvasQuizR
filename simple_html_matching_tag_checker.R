# check for matching tags
simple_html_matching_tag_checker= function(tags, opentags=NULL) {
  
  nlen= length(tags)
  opentags= NULL
  for (i1 in 1:nlen) {
    # if closing tag does not match parent: error
    if (stri_sub(tags[i1], 1, 1) == "/") {
      if ( is.null(opentags) | (opentags[1] != stri_sub(tags[i1],2))) {
        stop(sprintf("ERROR: UNMATCHED HTML TAG <%s>", tags[i1]))
      } else { # decrease level
        if (length(opentags) == 1) opentags= NULL 
        else opentags= opentags[2:length(opentags)]
      }
    } else { # increase the level
      opentags= c(tags[i1], opentags)
    }
  }
  if (!is.null(opentags)) stop(sprintf("ERROR: UNCLOSED TAGS <%s>", opentags))
}
