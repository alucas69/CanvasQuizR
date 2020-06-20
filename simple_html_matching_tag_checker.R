# check for matching tags
simple_html_matching_tag_checker= function(vs_tags) {
  nlen= length(vs_tags)
  # if only one line: no matching tag
  if (nlen == 1) stop(sprintf("ERROR: UNMATCHED HTML TAG <%s>", vs_tags[1]))
  # find matching closing tag: if not found, then error
  matches= (vs_tags == sprintf("/%s", vs_tags[1]))
  if (max(as.numeric(matches)) == 0) stop(sprintf("ERROR: UNMATCHED HTML TAG <%s>", vs_tags[1]))
  # there is a match; remove closest open-close tags, and proceed checking
  # remove matching tag, and then first tag
  if (nlen == 2) return() # we are done
  else {
    indexmatch= which(matches)[1]
    if (indexmatch == 2) simple_html_matching_tag_checker(vs_tags[3:nlen]) # adjacent tags
    else if (indexmatch == nlen) simple_html_matching_tag_checker(vs_tags[2:(nlen-1)]) # wrapper tags
    else { 
      # hierarchy continued
      simple_html_matching_tag_checker(vs_tags[2:(indexmatch-1)])
      simple_html_matching_tag_checker(vs_tags[(indexmatch+1):nlen])
    }
  }
}


