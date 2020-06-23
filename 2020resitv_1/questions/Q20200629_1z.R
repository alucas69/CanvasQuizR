Q20200629_1z <- function() {

  qtype= "ma"
    QuestionText <- matrix(c(
      # # regression
      # 1, "For a sample of students the following data are available: the individual's 
      # grade for a statistics exam, the individual's grade for a math exam and the 
      # number of hours used to study statistics. We want to test if there is any 
      # relation between the grade for statistics and the two other variables.",
      # two sample independent t test
      2, "For a sample of students the following data are available: the individual's 
      grade for a statistics exam, the individual's grade for a math exam. 
      We want to test if the means of these two variables are equal.",
      # F test
      3, "For a sample of students the following data are available: the individual's 
      grade for a statistics exam, the individual's grade for a math exam and the 
      number of hours used to study statistics. We want to test if the variances 
      of these two variables is the same.",
      # chi squared test
      4, "For a sample of students the following data are available: whether the 
      individual succeeded on the first attempt of the statistics exam, the logistics
      exam, and the mathematics exam. You want to test whether these variables are 
      related (i.e., dependent).",
      # ANOVA
      5, "For a sample of students, you observed their program (history, economics,
      business, sociology, physics) and their GPA on compulsory coursework. You want
      to test whether these GPAs have the same average across programs."),
      nrow = 2)
    QuestionText <- QuestionText[ , sample(1:ncol(QuestionText), 1)]
    CorrectAnswer <- QuestionText[1]
    QuestionText <- c(QuestionText[2],
                      "What is the correct approach for conducting your test? Give all
                      correct answers.")
    #
    # compute the answer and error margin
    #
    Answers <- matrix(c(
#      1, "Regression",
      2, "Independent 2-sample t-test",
      3, "F-test",
      4, "chi-squared-test",
      5, "ANOVA", 
      6, "Paired 2-sample t-test",
      7, "Chi-squared goodness-of-fit test"
    ), nrow = 2)
    Answers <- Answers[ , sample(1:ncol(Answers), 5)]
    Answers <- cbind(Answers, c(0, "None of the above"))
    Answers[1,] <- 0 + (as.numeric(Answers[1,]) == CorrectAnswer)
    if (max(as.numeric(Answers[1,])) == 0) Answers[1,ncol(Answers)] <- 1

    return(list(type=qtype, text=QuestionText, answer=Answers))    
}
