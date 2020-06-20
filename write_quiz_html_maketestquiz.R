write_quiz_html_maketestquiz= function() {
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
  return(testquiz)
}
