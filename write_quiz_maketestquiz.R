write_quiz_maketestquiz= function(qtype=NULL, blocks=3, questions=3, witherrors=FALSE) {

  testquiz = list()  
  for (blockcounter in 1:blocks) {
    testblock= list()
    for (questioncounter in 1:questions) {
      
      if (is.null(qtype)) type_i= sample(c("mc","ma","num","alpha","upl"))
      else type_i = qtype
      
      testquestion= list()
      testquestion$q= sprintf("Question %d.%d", blockcounter, questioncounter)
                              
      if (type_i == "mc") {
        testquestion$type= "mc"
        i1= sample(1:10, 1)
        i2= sample(1:10, 1)
        order= sample(1:3, 3)
        testquestion$text= sprintf("What is %d + %d ?", i1, i2)
        testquestion$answer= paste(c(i1*i2, i1-i2, i1+i2), sep="")[order]
        testquestion$correct= c(F,F,T)[order]
        if (witherrors) testquestion$correct= rep(F, length(testquestion$correct))
      }  else if (type_i == "ma") {
        testquestion$type= "ma"
        i1= sample(1:10, 1)
        i2= sample(1:10, 1)
        order= sample(1:4, 4)
        testquestion$text= sprintf("What is %d + %d ?", i1, i2)
        testquestion$answer= paste(c(i1*i2, i1-i2, i1+i2, i1+i2), sep="")[order]
        testquestion$correct= c(F,F,T,T)[order]
        if (witherrors) testquestion$correct= rep(F, length(testquestion$correct))
      }  else if (type_i == "mb") {
        testquestion$type= "mb"
        testquestion$text= c("Give a rhyme word for elephant: [elephant].", 
                             "And also give rhyme words for horse [horse] and for of course [ofcourse]")
        testquestion$answer= c("demonstrant", "remorse", "horse")
        testquestion$answernames= c("[elephant]", "[horse]", "[ofcourse]")
        if (witherrors) {
          testquestion$text= c(textquestion$text, "And a redundant [variable].")
          testquestion$answernames= c(testquestion$answernames, "[elephan]")
        }
      }  else if (type_i == "num") {
        testquestion$type= "num"
        i1= sample(1:10, 1)
        i2= sample(1:10, 1)
        order= sample(1:4, 4)
        testquestion$text= sprintf("What is %d + %d ?", i1, i2)
        testquestion$answer= matrix(rep(i1+i2,3), ncol=3)
        if (witherrors) testquestion$answer[2]= testquestion$answer - 1
      }  else if (type_i == "alph") {
        testquestion$type= "alph"
        testquestion$text= "Write your best essay on pi."
      }  else if (type_i == "upl") {
        testquestion$type= "upl"
        testquestion$text= "Upload an image of pi."
      }  else stop("ERROR: YOU ARE ASKING FOR AN UNCODED QUESTION")
      
      testblock= append(testblock, list(testquestion))
    }
    
    testquiz= append(testquiz, list(testblock))
  }
  return(testquiz)
}
