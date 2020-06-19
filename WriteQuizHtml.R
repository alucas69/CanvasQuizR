# required libraries
library("stringi")


# indent by i_indent * i_indentspace spaces
sprintf_indent= function(vs_text, i_indent=1, i_indentspace=3) {
  if ((i_indent >= 1) & (i_indentspace >= 1))
      vs_text= sprintf(sprintf("%%%ds%%s", as.integer(i_indent) * as.integer(i_indentspace)),
                       " ", vs_text)
  return(vs_text)
}


# write each row of vector of strings vs_text into html wrapper s_wrappername with s_wrappertag 
# if block==FALSE. Do so inline if inline=TRUE to get a more condensed html file.
# if block == TRUE, write the whole vector of strings into the wrapper (rather than each
# element separately)
write_in_wrapper= function(vs_text, s_wrappername, s_wrappertag="", inline=TRUE, block=FALSE) {
  if (block) return(c(
    sprintf("<%s %s>", s_wrappername, s_wrappertag),
    sprintf_indent(vs_text),
    sprintf("</%s>", s_wrappername)
  )) else {
    if (inline) return(sprintf("<%s %s>%s</%s>", s_wrappername, s_wrappertag, vs_text, s_wrappername))
    else return(rbind(
      sprintf("<%s %s>", s_wrappername, s_wrappertag),
      rbind(sprintf_indent(vs_text), sprintf("</%s>", s_wrappername))
    ))
  }
}


write_as_html_ul= function(vs_text, vs_tags=NULL) {
  # initialize tags and check tags dimension
  if (is.null(vs_tags)) vs_tags= rep("", length(vs_text))
  else if (length(vs_tags) == length(vs_text)) vs_tags= sprintf(" %s", vs_tags)
  else stop(sprintf("Wrong length of tags (%d) versus text (%d) in write_as_html_ul", length(vs_tags), length(vs_text)))
  
  # make list entries
  output= write_in_wrapper(vs_text, "li", vs_tags)
  
  # return list
  return( write_in_wrapper(output, "ul", block = TRUE) )
}


write_as_html_table= function(m_text, vs_celltags=NULL, firstrowheader=FALSE,
                              tablestyle="style=\"border:2px solid black\"",
                              tableheaderstyle="style=\"border:2px solid black; background-color:#B1B1B1\"") {
  
  # initialize tags and check tags dimension
  if (!is.null(vs_celltags)) warning("Tags not implemented yet in write_as_html_table")

  # make table
  output= NULL
  for (i1 in 1:nrow(m_text)) {
    outputline= NULL
    wrapper= c("td", "th")[1 + (firstrowheader & (i1==1))]
    for (i2 in 1:ncol(m_text)) outputline= c(outputline, write_in_wrapper(m_text[i1,i2], wrapper))
    output= c(output, 
              write_in_wrapper(outputline, "tr", 
                               s_wrappertag = c("", tableheaderstyle)[1 + (firstrowheader & (i1==1))],
                               block = TRUE))
  }

  # return list
  return(write_in_wrapper(output, "table", s_wrappertag = tablestyle, block = TRUE))
}


# write colored true/false ul list
write_as_html_ul_color= function(vs_text, vb_tags, truecolor="green", falsecolor="red") {
  if (length(vs_text) != length(vb_tags)) stop("tags not same length as text in write_as_html_ul_color")
  if (!is.logical(vb_tags)) stop("tags does not hold booleans for indicating true/false")
  vs_tags= sprintf("style=\"color:%s; list-style-type:%s\"", 
                   c(falsecolor, truecolor)[1+as.numeric(vb_tags)],
                   c("circle", "disc")[1+as.numeric(vb_tags)])
  return( write_as_html_ul(vs_text, vs_tags) )
}


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
    
write_quiz_html_mc= function(question, minimumcorrect=1, maximumcorrect=1) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # write color coded answers
  output$answer= write_as_html_ul_color(question$answer, as.logical(question$correct))
  if (sum(as.integer(question$correct)) < minimumcorrect) 
    output$answer= c(write_in_wrapper("WARNING: TOO FEW CORRECT ANSWERS!!", "strong", s_wrappertag = "style=\"color:red\""), output$answer)
  if (sum(as.integer(question$correct)) > maximumcorrect) 
    output$answer= c(write_in_wrapper("WARNING: TOO MANY CORRECT ANSWERS!!", "strong", s_wrappertag = "style=\"color:red\""), output$answer)
  
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}


write_quiz_html_ma= function(question) {
  return(write_quiz_html_mc(question, minimumcorrect = 1, maximumcorrect = length(question$answer)-1 ))
}


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


write_quiz_html_mb= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()

  # write the question part as paragraphs
  mb_core= write_quiz_html_mb_highlight_variables(question$text)
  output$question= write_in_wrapper(mb_core$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # write answer part
  output$answer= write_quiz_html_mb_highlight_variables(paste(
    question$answernames, ": ", question$answer, sep=""))$text
  correct= rep(TRUE, length(question$answernames))
  notused= which(!(question$answernames %in% mb_core$variables))
  notdefined= which(!(mb_core$variables %in% question$answernames))
  if (length(notused) > 0) {
    output$answer[notused]= paste(
      write_in_wrapper("WARNING: NOT USED!!","strong"), output$answer[notused])
    correct[notused]= FALSE
  }
  if (length(notdefined) > 0) {
    output$answer= c(output$answer, 
                     paste(write_in_wrapper("WARNING: NOT DEFINED!!", "strong"), 
                           write_quiz_html_mb_highlight_variables(mb_core$variables[notdefined])$text)
    )
    correct= c(correct, rep(FALSE, length(notdefined)))
  }
  output$answer= write_as_html_ul_color(output$answer, correct, truecolor = "black")
  
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}


write_quiz_html_num= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  # check answers
  correct = 
    # lower bound below answer
    (question$answer[ , 2] <= question$answer[ , 1]) &
    # answer below upper bound
    (question$answer[ , 1] <= question$answer[ , 3])
  # write answers in table
  output$answer= write_as_html_table(
    rbind(
      c("Value", "Lower", "Upper", "Check"),
      cbind(
        matrix(c(question$answer), ncol = 3),
        c(write_in_wrapper("WARNING: BOUND VIOLATION", "strong", "style=\"color:red\""), "")[1+correct]
      )
    ),
    firstrowheader = TRUE
  )
  
  # output the table
  output= write_three_part_table(output$heading, output$question, output$answer)
  return(output)
}


write_quiz_html_alph= function(question) {
  # initialize: three part table with heading, question, answers
  output= list()
  
  # write the question part as paragraphs
  output$question= write_in_wrapper(question$text, "p")
  # write the table heading
  output$heading= write_in_wrapper(c(question$q, sprintf("(question type: %s)", question$type)), "p")
  
  # output the table
  output= write_three_part_table(output$heading, output$question)
  return(output)
}


write_quiz_html_upl= function(question) {
  return(write_quiz_html_alph(question))
}


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


# l_quiz is a quiz list of blocks, each block a list of questions
write_quiz_html= function(s_filename, l_quiz, s_dirname=".") {
  # compile the quiz
  output= NULL
  for (i0 in 1:length(l_quiz)) {
    block= l_quiz[[i0]]
    output= c(output, write_in_wrapper(write_in_wrapper(sprintf("Here starts block %d", i0), "strong"), "h1"))
    for (i1 in 1:length(block)) {
      question= block[[i1]]
      if (question$type == "mc") output= c(output, write_quiz_html_mc(question))
      else if (question$type == "ma") output= c(output, write_quiz_html_ma(question))
      else if (question$type == "mb") output= c(output, write_quiz_html_mb(question))
      else if (question$type == "num") output= c(output, write_quiz_html_num(question))
      else if (question$type == "alph") output= c(output, write_quiz_html_alph(question))
      else if (question$type == "upl") output= c(output, write_quiz_html_upl(question))
      output= c(output, "&nbsp;<br>&nbsp;<br>")
    }
  }
  write_quiz_html_file(s_filename, output, s_dirname)  
}



write_quiz_html_test= function() {
  testquestion= list()
  testquestion$q= "Question 1"
  testquestion$type= "mc"
  testquestion$text= c("What is","the meaning of life?")
  testquestion$answer= c("yes","no","pi")
  testquestion$correct= c(F,F,F)
  testblock= list(testquestion)

  testquestion$q= "Question 2"
  testquestion$type= "ma"
  testquestion$correct= c(T,T,T)
  testblock= append(testblock, list(testquestion))

  testquestion$q= "Question 3"
  testquestion$type= "num"
  testquestion$answer= matrix(round(rnorm(12), digits = 3), ncol = 3)
  for (i1 in 1:nrow(testquestion$answer)) testquestion$answer[i1,] = sort(testquestion$answer[i1,])
  testquestion$answer= testquestion$answer[ ,c(2,1,3)]
  testquestion$answer[3,1] = testquestion$answer[3,2] - 1
  testblock= append(testblock, list(testquestion))

  testquiz= list(testblock)

  testquestion$q= "Question 4"
  testquestion$type= "mb"
  testquestion$text= c("what is [the] meaning [of] [lif]e?")
  testquestion$answer= c("geen","idee","hoep")
  testquestion$answernames= c("[the]", "[of]", "[life]")
  testquestion$correct= c(F,F,T)
  testblock= list(testquestion)

  testquestion$q= "Question 5 (essay)"
  testquestion$type= "alph"
  testblock= append(testblock, list(testquestion))
  
  testquestion$q= "Question 6 (upload)"
  testquestion$type= "upl"
  testblock= append(testblock, list(testquestion))
  
  testquiz= append(testquiz, list(testblock))
  write_quiz_html("write_quiz_html_test.html", testquiz)
}

