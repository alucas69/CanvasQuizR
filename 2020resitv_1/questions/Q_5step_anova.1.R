# ANOVA question (5 step plan) with 3 samples 

########################################
# Q_5step_anova.1_gendata()
#
# Purpose:
#   Generate data and produce tables
#
# Inputs:
#
# Return value, list with:
#   $df_sample: dataframe with data (Y, X)
#   $results: list of 4 lists,
#     $aov: the aov output list
#     $aov_summary: the aov summary output list
#     $aov_levene: the aov leveneTest output list
#     $aov_tukey: the aov TukeyHSD output list
#   $tables: list of 4 tables, 
#     $aov, with output of aov print
#     $aov_summary, with output of summary(aov) print
#     $aov, with output of leveneTest(aov) print
#     $aov, with output of TukeyHSD(aov) print

Q_5step_anova.1_gendata <- function(f_cc=1){
  # generate sample sizes
  nr_samples= 3
  vi_N= sample(10:20, nr_samples, replace = TRUE)
  i_totn= sum(vi_N)
  
  # generate categories
  vs_X= NULL
  for (i1 in 1:nr_samples) vs_X= c(vs_X, rep(i1, vi_N[i1]))
  # generate samples with mean distances c/sqrt(totn)
  f_c= 0.5 * f_cc * sample(-5:5, nr_samples, replace = TRUE) / sqrt(i_totn)
  vf_Y= rnorm(i_totn) + f_c[vs_X]
  vs_X= factor(sprintf("sample%d", vs_X))
  df_sample= data.frame(y=vf_Y, samples=vs_X)

  # generate anova output
  l_aov= aov(y ~ samples, data=df_sample)
  
  # generate tables
  l_tables= myprettyaovprint(l_aov)

  return(list(sample=df_sample, vi_N=vi_N, 
              results=list(aov=l_aov, aov_summary=summary(l_aov), aov_levene=leveneTest(l_aov), aov_tukey=TukeyHSD(l_aov)), 
              tables=l_tables))
}

########################################
Q_5step_anova.1_core <- function(f_cc=1) {
  Setting= list(
    list(
      Topic="You study the tenure times of temporary staff.",
      Subject= "tenure times",
      OverallMean= 50,
      Text1= "You sampled the following groups",
      Groups= c("managers", "blue-collar workers", "white-collar workers")
    )
  )
  Setting= sample(Setting, 1)[[1]]

  # generate the data
  Sample= Q_5step_anova.1_gendata(f_cc)
  Sample$sample$y1= Sample$sample$y + Setting$OverallMean
  
  # generate test settings
  dAlpha= sample(c(0.1, 0.05, 0.01), 1)
  
  # Write intro
  TotalIntro= sprintf("%s You gathered %s for %d different samples:", Setting$Topic, Setting$Subject, length(Sample$vi_N))
  Setting$Groups= Setting$Groups[sample(1:length(Setting$Groups), length(Sample$vi_N))]
  for (i1 in 1:length(Setting$Groups)) TotalIntro= paste(TotalIntro, c(" ",", ")[1+(i1>1)], sprintf("%s (sample %d)", Setting$Groups[i1], i1), sep="")
  Setting$TotalIntro= paste(TotalIntro, sprintf(". You perform the 5-step test plan for an ANOVA with $\\alpha=%4.2f$.", Q$alpha), sep="")

  return(list(setting=Setting, sample=Sample, alpha=dAlpha))
}


########################################
Q_5step_anova.1_step1a <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()
  
  # initialize
  qtype= "mc"
  Hypothesis= sample(c(0,1), 1)
  
  # question text
  Text= c(Q$setting$TotalIntro, write_in_wrapper(sprintf("What is your %s hypothesis?", c("*null*","*alternative*")[Hypothesis+1]), "b", s_wrappertag="style=\"color:blue\""))
  
  # answers
  Answers= matrix(c(
    0, "the means of all populations are the same",
    1, "the means of at least two populations are different",
    -1, "the means of all three populations are different",
    -1, "the means of all samples are the same",
    -1, "the means of at least two samples are different",
    -1, "the means of all three samples are different",
    -1, "the variances of all populations are the same",
    -1, "the variances of at least two populations are different",
    -1, "the variances of all three populations are different",
    -1, "the variances of all samples are the same",
    -1, "the variances of at least two samples are different",
    -1, "the variances of all three samples are different"
  ), nrow=2)

  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==Hypothesis), iAlt=7, mincorrect = 1)

  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}  

########################################
Q_5step_anova.1_step2a <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()
  
  # initialize
  qtype= "mc"
  
  # question text
  Text= c(Q$setting$TotalIntro, write_in_wrapper(sprintf("What is your statistic and rejection region?"), "b", s_wrappertag="style=\"color:blue\""))
  
  # answers
  Answers= matrix(c(
    1, "ANOVA F-test",
    1, "F-test",
    0, "chi-squared test statistic, reject for large values",
    0, "two-sample (paired samples) t-test for differences in means",
    0, "two-sample (independent samples) t-test for differences in means",
    0, "two-sample (independent samples) t-test for differences in proportions"
  ), nrow=2)
  Answers= cbind(
    rbind(Answers[1,], sprintf("%s, reject for *large* values", Answers[2,])),
    rbind(0, sprintf("%s, reject for *small* values", Answers[2,])),
    rbind(0, sprintf("%s, reject for *large and small* values", Answers[2,]))
  )
  
  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=7, mincorrect = 0)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


########################################
Q_5step_anova.1_step3a <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()
  
  # initialize
  qtype= "num"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("The number of observations in the three samples equals $n_1=%d$, $n_2=%d$, and $n_3=%d$. You perform an F-test.", Q$sample$vi_N[1], Q$sample$vi_N[2], Q$sample$vi_N[3]))
  Text= c(Text, write_in_wrapper(sprintf("What are the degrees of freedom corresponding to your test statistic under the null hypothesis? If there is more than one degree-of-freedom parameter, separate the two by a decimal point, e.g., 5 and 12 becomes 5.12. Do not worry if Canvas removes a trailing zero, e.g. replacing 5.120 by 5.12."), "b", s_wrappertag="style=\"color:blue\""))

  # answers
  Answers= matrix(rep(as.double(sprintf("%d.%d", 2, sum(Q$sample$vi_N)-3)), 3), ncol=3)

  # return
  return(list(type=qtype, text=Text, answer=Answers))  
  
}



########################################
Q_5step_anova.1_step3b <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()
  
  # initialize
  qtype= "ma"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("The number of observations in the three samples equals $n_1=%d$, $n_2=%d$, and $n_3=%d$. You perform an F-test.", Q$sample$vi_N[1], Q$sample$vi_N[2], Q$sample$vi_N[3]))
  Text= c(Text, write_in_wrapper(sprintf("Which of the below assumptions are required for your test statistic to have an F-distribution under the null hypothesis? Choose *ALL* correct answers."), "b", s_wrappertag="style=\"color:blue\""))
  
  # answers
  Answers= matrix(c(
    1, "the populations are normally distributed",
    1, "all populations have the same variance",
    0, "all samples have the same variance",
    0, "all samples are normally distributed",
    0, "the ANOVA F-test should be large and significant",
    0, "the ANOVA F-test should be sufficiently small",
    0, "the sample sizes should all be greater than 30",
    0, "the sample sizes should all be greater than 10",
    0, "the sum of squared residuals should not exceed the F-critical value"
  ), nrow=2)

  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=7, mincorrect = 0)

  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


########################################
Q_5step_anova.1_step3c <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()
  
  # initialize
  qtype= "ma"
  alphaL= 0.05
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("Using a Levene test, you test at $\\alpha_{Levene}=%4.2f$ whether the variances are equal across the %d populations corresponding to the different groups. The output of this test is given below.", alphaL, length(Q$sample$vi_N)))
  Text= c(Text, write_in_wrapper(Q$sample$tables$aov_levene, "pre"))
  Text= c(Text, write_in_wrapper(sprintf("What is your conclusion regarding the variances of the %d groups and why? Check *ALL* correct answers.", length(Q$sample$vi_N)), "b", s_wrappertag="style=\"color:blue\""))
  
  # answers
  Answers= matrix(c(
    2, "we *reject* the null hypothesis of equal variances, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "we do *not reject* the null hypothesis of equal variances, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "we *reject* the null hypothesis of equal variances, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$",
    1, "we do *not reject* the null hypothesis of equal variances, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$",
    2, "at least 2 variances are different, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "at least 2 variances are different, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "at least 2 variances are different, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$",
    1, "at least 2 variances are different, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$",
    0, "all variances are different, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "all variances are different, as the Levene test has a p-value *smaller* than $\\alpha_{Levene}$",
    0, "all variances are different, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$",
    0, "all variances are different, as the Levene test has a p-value *larger* than $\\alpha_{Levene}$"
  ), nrow=2)
  reject= (Q$sample$results$aov_levene$`Pr(>F)`[1] < alphaL)
  
  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1+as.integer(reject)), iAlt=7, mincorrect = 0)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}



########################################
Q_5step_anova.1_step5a <- function() {
  # generate the question
  Q= Q_5step_anova.1_core()

  # initialize
  qtype= "ma"
  
  # question text
  Text= c(Q$setting$TotalIntro, write_in_wrapper(paste(Q$sample$tables$aov, Q$sample$tables$aov_summary, sep="\n"), "pre"))
  Text= c(Text, write_in_wrapper(sprintf("What is your conclusion regarding the means of the %d groups and why? Check *ALL* correct answers.", length(Q$sample$vi_N)), "b", s_wrappertag="style=\"color:blue\""))

  # answers
  Answers= matrix(c(
    2, "we *reject* the null hypothesis of equal means, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "we do *not reject* the null hypothesis of equal means, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "we *reject* the null hypothesis of equal means, as the ANOVA F-test has a p-value *larger* than $\\alpha$",
    1, "we do *not reject* the null hypothesis of equal means, as the ANOVA F-test has a p-value *larger* than $\\alpha$",
    2, "at least 2 means are different, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "at least 2 means are different, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "at least 2 means are different, as the ANOVA F-test has a p-value *larger* than $\\alpha$",
    1, "at least 2 means are different, as the ANOVA F-test has a p-value *larger* than $\\alpha$",
    0, "all means are different, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "all means are different, as the ANOVA F-test has a p-value *smaller* than $\\alpha$",
    0, "all means are different, as the ANOVA F-test has a p-value *larger* than $\\alpha$",
    0, "all means are different, as the ANOVA F-test has a p-value *larger* than $\\alpha$"
  ), nrow=2)
  reject= (Q$sample$results$aov_summary[[1]][1,"Pr(>F)"] < Q$alpha)
  
  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1+as.integer(reject)), iAlt=7, mincorrect = 0)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


########################################
Q_5step_anova.1_step5b <- function() {
  # generate the question with significant ANOVA
  reject= FALSE
  while (!reject) {
    Q= Q_5step_anova.1_core(f_cc=3)
    reject= (Q$sample$results$aov_summary[[1]][1,"Pr(>F)"] < Q$alpha)
  }
  alphaT= 0.05
  
  # initialize
  qtype= "ma"
  
  # question text
  Text= c(Q$setting$TotalIntro, write_in_wrapper(Q$sample$tables$aov, "pre"))
  Text= c(Text, write_in_wrapper(Q$sample$tables$aov_summary, "pre"))
  Text= c(Text, sprintf("Next, you applied a Tukey post-hoc analysis with $\\alpha_{Tukey}=%4.2f$. You have the following table.", alphaT))
  Text= c(Text, write_in_wrapper(paste(Q$sample$tables$aov_tukey, sep="\n"), "pre"))
  Text= c(Text, write_in_wrapper(sprintf("What is your conclusion regarding the means of the %d groups at $\\alpha_{Tukey}=%4.2f$? Check *ALL* correct answers. Remember: you never *accept* a null hypothesis.", length(Q$sample$vi_N), alphaT), "b", s_wrappertag="style=\"color:blue\""))
  
  # answers
  Answers= matrix(c(
    0, "mean of group 1 is unequal to that of of group 2",
    0, "mean of group 1 is unequal to that of of group 3",
    0, "mean of group 2 is unequal to that of of group 3",
    0, "there is insufficient evidence to conclude that the mean of group 1 and group 2 are different",
    0, "there is insufficient evidence to conclude that the mean of group 1 and group 3 are different",
    0, "there is insufficient evidence to conclude that the mean of group 2 and group 3 are different",
    0, "mean of group 1 is equal to that of of group 2",
    0, "mean of group 1 is equal to that of of group 3",
    0, "mean of group 2 is equal to that of of group 3"
  ), nrow=2)
  rownames(Q$sample$results$aov_tukey)  
  # get positions
  pos= which(!is.na(stri_locate(rownames(Q$sample$results$aov_tukey[[1]]), regex=sprintf("[%s].+[%s]", "12", "12"))[,1]))
  pos= c(pos, which(!is.na(stri_locate(rownames(Q$sample$results$aov_tukey[[1]]), regex=sprintf("[%s].+[%s]", "13", "13"))[,1])))
  pos= c(pos, which(!is.na(stri_locate(rownames(Q$sample$results$aov_tukey[[1]]), regex=sprintf("[%s].+[%s]", "23", "23"))[,1])))

  # correct
  for (i1 in 1:length(pos)) {
    reject= (Q$sample$results$aov_tukey[[1]][pos[i1], "p adj"] < alphaT)
    if (reject) Answers[1, pos[i1]]= 1 else Answers[1,3+pos[i1]]= 1
  }
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=7, mincorrect = 0, alphorder=TRUE)
  
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


