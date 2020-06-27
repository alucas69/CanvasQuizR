# 2-sample pi difference question (5 step plan)

########################################
# Q_5step_2pi.1_gendata()
#
# Purpose:
#   Generate data and produce tables
#
# Inputs:
#
# Return value, list with:
#   $df_sample: dataframe with data (Y, X)
  # $results: list of 4 lists,
  #   $aov: the aov output list
  #   $aov_summary: the aov summary output list
  #   $aov_levene: the aov leveneTest output list
  #   $aov_tukey: the aov TukeyHSD output list
  # $tables: list of 4 tables,
  #   $aov, with output of aov print
  #   $aov_summary, with output of summary(aov) print
  #   $aov, with output of leveneTest(aov) print
  #   $aov, with output of TukeyHSD(aov) print

Q_5step_2pi.1_gendata <- function(f_cc= 1){
  # generate sample sizes
  nr_samples= 3
  vi_N= 10 * sample(3:8, nr_samples)
  i_totn= sum(vi_N)
  
  # generate two samples with the same pi, one with a different pi
  vd_Pi= rep(0.1 * sample(3:7, 1), 2)
  d_Pi2= min(0.2, 0.8, vd_Pi+ 0.1 * f_cc / sqrt(max(vi_N)))
  vd_Pi= c(vd_Pi, d_Pi2)
  
  # generate data until at least 10 draws in each bucket
  vi_Data= rep(0, nr_samples)
  drawcounter= 1
  while ((min(vi_Data) < 10) | (min(vi_N - vi_Data) < 10)) {
    for (i1 in 1:nr_samples) vi_Data[i1]= rbinom(1, vi_N[i1], prob=vd_Pi[i1])
    if ((drawcounter %% 1000) == 0) warning("WARNING: Q_5step_2pi.1_gendata() drawing for a long time", immediate. = TRUE)
    drawcounter= drawcounter + 1
  } 
  
  # summary table
  Tabley1y2= cbind(vi_N[1:2], vi_Data[1:2])
  rownames(Tabley1y2)= c("sample1", "sample2")
  colnames(Tabley1y2)= c("n", "count")
  Tabley1x1= cbind(vi_N[c(1,3)], vi_Data[c(1,3)])
  rownames(Tabley1x1)= c("sample1", "sample2")
  colnames(Tabley1x1)= c("n", "count")
  
  return(list(
    sample=vi_Data, vi_N=vi_N, vd_Pi=vd_Pi,
    tables=list(table_yy=myprettytableprint(Tabley1y2, digits = 0), table_yx=myprettytableprint(Tabley1x1, digits = 0))
  ))
}


########################################
Q_5step_2pi.1_core <- function(f_cc=1) {
  Setting= list(
    list(
      Topic="You study the buying behavior for your product. You counted the number of times your product was sold in samples from two different populations.",
      Subject="the probability of buying your product",
      Subject2="the number of times the product is bought",
      Subject3="the number of times the product is *not* bought",
      Text1= "",
      Groups= c("young people", "elderly people")
    )
  )
  Setting= sample(Setting, 1)[[1]]

  # generate the data
  Sample= Q_5step_2pi.1_gendata(f_cc)
  
  # alternative
  iDirection= sample(c(-1,0,1), 1)
  Setting$Comparative= c("less", "the same", "greater")[2+iDirection]
  Setting$Than= c("than", "as", "than")[2+iDirection]
  
  # generate test settings
  dAlpha= sample(c(0.1, 0.05, 0.01), 1)
  
  # Write intro
  TotalIntro= sprintf("%s You considered the following samples: ", Setting$Topic)
  # randomize group order if uncommented
  Setting$Groups= Setting$Groups[sample(1:length(Setting$Groups))]
  for (i1 in 1:length(Setting$Groups)) TotalIntro= paste(TotalIntro, c(" ",", ")[1+(i1>1)], sprintf("%s (sample %d)", ColorBold(Setting$Groups[i1]), i1), sep="")
  Setting$TotalIntro= c(
    paste(TotalIntro, ".", sep=""),
    sprintf("You perform a 5-step testing plan at %s to test whether %s is %s for %s %s for %s.", 
            ColorBold(sprintf("$\\alpha=%4.2f$", dAlpha)), Setting$Subject, ColorBold(Setting$Comparative), Setting$Groups[1], Setting$Than, Setting$Groups[2])
  )

  return(list(setting=Setting, sample=Sample, alpha=dAlpha, direction=iDirection))
}


########################################
Q_5step_2pi.1_step1a <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  qtype= "mc"
  Hypothesis= sample(c(0,1), 1)
  
  # question text
  Text= c(
    Q$setting$TotalIntro, 
    write_in_wrapper(sprintf("What is your %s hypothesis?", c("*null*","*alternative*")[Hypothesis+1]), "b", s_wrappertag="style=\"color:blue\"")
  )
  
  # answers (correct one for "Q$direction.Hypothesis" case)
  Answers= matrix(c(
    -3, "$H_%d: \\mu_1 \\geq \\mu_2$",
    -3, "$H_%d: \\mu_1 = \\mu_2$",
    -3, "$H_%d: \\mu_1 \\ne \\mu_2$",
    -3, "$H_%d: \\mu_1 \\leq \\mu_2$",
    -3, "$H_%d: \\mu_1 = \\mu_2 = \\mu_0$",
    -3, "$H_%d: \\overline{x}_1 \\geq \\overline{x}_2$",
    -3, "$H_%d: \\overline{x}_1 \\ne \\overline{x}_2$",
    -3, "$H_%d: \\overline{x}_1 = \\overline{x}_2$",
    -3, "$H_%d: \\overline{x}_1 \\leq \\overline{x}_2$",
    1.1, "$H_%d: \\pi_1 > \\pi_2$",
    -1.0, "$H_%d: \\pi_1 \\geq \\pi_2$",
    0.0, "$H_%d: \\pi_1 = \\pi_2$",
    0.1, "$H_%d: \\pi_1 \\ne \\pi_2$",
    1.0, "$H_%d: \\pi_1 \\leq \\pi_2$",
    -1.1, "$H_%d: \\pi_1 < \\pi_2$",
    -3, "$H_%d: p_1 > p_2$",
    -3, "$H_%d: p_1 \\geq p_2$",
    -3, "$H_%d: p_1 = p_2$",
    -3, "$H_%d: p_1 \\ne p_2$",
    -3, "$H_%d: p_1 \\leq p_2$",
    -3, "$H_%d: p_1 < p_2$"
  ), nrow=2)
  Answers[2,]= sprintf(Answers[2,], Hypothesis)

  # correct
  Answers= answer_select(Answers[2,], (as.double(Answers[1,])==as.double(sprintf("%d.%d", Q$direction, Hypothesis))), iAlt=7, mincorrect=1)

  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}  

########################################
Q_5step_2pi.1_step2a <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  qtype= "mc"
  
  # question text
  Text= c(Q$setting$TotalIntro, ColorBold("What is your statistic and rejection region?"))

  # answers
  Answers= matrix(c(
    0, "$z = (\\overline{x} - 0) / (\\sigma/\\sqrt{n})$",
    0, "$t = (\\overline{x} - 0) / (s/\\sqrt{n})$",
    0, "$t = (\\overline{x}_1 - \\overline{x}_2) / \\sqrt{s_1^2/n_1 + s_2^2/n_2}$",
    0, "$z = (p - \\pi_0) / \\sqrt{\\pi_0 (1-\\pi_0) / n}$",
    1, "$z = (p_1 - p_2) / \\sqrt{p_1(1-p_1)/n_1 + p_2(1-p_2)/n_2}$",
    1, "$z = (p_1 - p_2) / \\sqrt{p_c(1-p_c)/n_1 + p_c(1-p_c)/n_2}$ with $p_c = (x_1+x_2)/(n_1+n_2)$"
  ), nrow=2)
  tmp= as.logical(as.integer(Answers[1,]))
  Answers= cbind(
    rbind(as.integer(tmp & (Q$direction==1)), sprintf("%s, reject for *large* values", Answers[2,])),
    rbind(as.integer(tmp & (Q$direction==0)), sprintf("%s, reject for *small and large* values", Answers[2,])),
    rbind(as.integer(tmp & (Q$direction==-1)), sprintf("%s, reject for *small* values", Answers[2,]))
  )
  
  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=8, mincorrect = 1, maxcorrect=1, alphorder = TRUE, addnone = FALSE)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


########################################
Q_5step_2pi.1_step3a <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  qtype= "mc"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You decide to use the test statistic $z = (p_1 - p_2)/\\sqrt{p_c(1-p_c)/n_1 + p_c(1-p_c)/n_2}$ with $p_c = (x_1+x_2)/(n_1+n_2)$. The summary of the data is as follows: <pre>%s</pre>", Q$sample$tables$table_yy))
  Text= c(Text, ColorBold("What is the distribution of $z$ under the null hypothesis, assuming all relevant conditions hold?"))
  
  # answers
  vi_N= Q$sample$vi_N[1:2]
  Answers= matrix(c(
    1, "normal distribution",
    0, sprintf("t distribution with %d degrees of freedom", sum(vi_N)-2),
    0, sprintf("t distribution with %d degrees of freedom", sum(vi_N)-1),
    0, sprintf("F distribution with %d and %d degrees of freedom", 2, sum(vi_N)-2),
    0, sprintf("$\\chi^2$ distribution with %d degrees of freedom", sum(vi_N)-2),
    0, sprintf("$\\chi^2$ distribution with %d degrees of freedom", sum(vi_N)-1)
  ), nrow=2)
  
  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=5)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}



########################################
Q_5step_2pi.1_step3b <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  qtype= "ma"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You decide to use the test statistic $z = (p_1 - p_2)/\\sqrt{p_c(1-p_c)/n_1 + p_c(1-p_c)/n_2}$ with $p_c = (x_1+x_2)/(n_1+n_2)$. The summary of the data is as follows: <pre>%s</pre>", Q$sample$tables$table_yy))
  Text= c(Text, ColorBold("Indicate all of the assumptions below needed for $z$ to have (approximately) a normal distribution under the null hypothesis."))
  
  # answers
  Answers= matrix(c(
    0, "the populations are normally distributed",
    0, "the samples are approximately normally distributed",
    0, "at least one of the populations is normally distributed",
    0, sprintf("%s in either sample 1 or sample 2 should be 10 or larger", Q$setting$Subject2),
    0, sprintf("%s in either sample 1 or sample 2 should be 10 or larger", Q$setting$Subject3),
    1, sprintf("%s in both sample 1 and sample 2 should be 10 or larger", Q$setting$Subject2),
    1, sprintf("%s in both sample 1 and sample 2 should be 10 or larger", Q$setting$Subject3)
  ), nrow=2)

  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1), iAlt=5)

  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


########################################
Q_5step_2pi.1_step3c <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  iDigits= 2
  qtype= "num"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You decide to use the test statistic $z = (p_1 - p_2)/\\sqrt{p_c(1-p_c)/n_1 + p_c(1-p_c)/n_2}$ with $p_c = (x_1+x_2)/(n_1+n_2)$ to test $H_0: \\pi_1 %s \\pi_2$. The summary of the data is as follows: <pre>%s</pre>", c("\\geq", "=", "\\leq")[2+Q$direction], Q$sample$tables$table_yy))
  Text= c(
    Text, 
    ColorBold("Assuming $z$ has a normal distribution under the null hypothesis, what is the critical value for $z$?"),
    DigitDecimalWarning(iDigits)
  )
  
  # answers
  Answers= qnorm(1 - Q$alpha/(1 + (Q$direction==0)))
  Answers= floor(100 * Answers)/100
  if (Q$direction==-1) Answers= -Answers
  Answers= matrix(sort(Answers + 0.01 * c(0, -1, 1)), nrow=1)
  Answers[, c(1,2)]= Answers[, c(2,1)]
  
  # return
  return(list(type=qtype, text=Text, answer=Answers))  
}



########################################
Q_5step_2pi.1_step4a <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  iDigits= 3
  qtype= "num"
  
  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You decide to use the test statistic $z = (p_1 - p_2)/\\sqrt{p_c(1-p_c)/n_1 + p_c(1-p_c)/n_2}$ with $p_c = (x_1+x_2)/(n_1+n_2)$ to test $H_0: \\pi_1 %s \\pi_2$. The summary of the data is as follows: <pre>%s</pre>", c("\\geq", "=", "\\leq")[2+Q$direction], Q$sample$tables$table_yx))
  Text= c(
    Text, 
    ColorBold("Compute the test statistic."),
    DigitDecimalWarning(iDigits)
  )
  
  # answers (using the Y1 and X1 sample) !!!
  n1= Q$sample$vi_N[1]
  n2= Q$sample$vi_N[3]
  p1= Q$sample$sample[1] / n1
  p2= Q$sample$sample[3] / n2
  pc= (n1*p1 + n2*p2) / (n1 + n2)
  dZ= (p1 - p2) / sqrt(pc * (1-pc) * (1/n1 + 1/n2))
  Answers= matrix(dZ + 3 * (10^(-iDigits)) * c(0, -1, 1), nrow=1)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers))  
}



########################################
Q_5step_2pi.1_step4b <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()
  
  # initialize
  iDigits= 4
  qtype= "num"
  
  # z statistic
  dZ= round(qnorm(sample(81:99, 1)/100), digits = 2)
  if (Q$direction == -1) dZ=-dZ

  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You test $H_0: \\pi_1 %s \\pi_2$ using a test statistic that is normally distributed under the null hypothesis. Based on your sample, you computed $z_{calc} = %4.2f$.", c("\\geq", "=", "\\leq")[2+Q$direction], dZ))
  Text= c(
    Text, 
    ColorBold("Compute the p-value of your test statistic."),
    DigitDecimalWarning(iDigits)
  )
  
  # answers
  Answers= pnorm( dZ, lower.tail = (Q$direction == -1))
  Answers= matrix(Answers + 7 * (10^(-iDigits)) * c(0, -1, 1), nrow=1)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers))  
}



########################################
Q_5step_2pi.1_step5a <- function() {
  # generate the question
  Q= Q_5step_2pi.1_core()

  # initialize
  qtype= "ma"
  
  # z statistic
  dZ= round(qnorm(1 - Q$alpha + Q$alpha * sample(c(-1,1),1) * runif(1, min=0.2, max=0.8)), digits = 2)
  dZcrit= round(qnorm(1 - Q$alpha), digits = 2)
  if (Q$direction == -1) {
    dZ= -dZ
    sRegion= sprintf("You determined your critical value to be %4.2f.", -dZcrit)
    reject= (dZ < -dZcrit)
  } else if (Q$direction == 1) {
    sRegion= sprintf("You determined your critical value to be %4.2f.", dZcrit)
    reject= (dZ > dZcrit)
  } else {
    dZ= sample(c(-1,1), 1) * dZ
    sRegion= sprintf("You determined your critical values to be %4.2f and %4.2f.", -dZcrit, dZcrit)
    reject= ((dZ < -dZcrit) | (dZ > dZcrit))
  }

  # question text
  Text= c(Q$setting$TotalIntro, sprintf("You test $H_0: \\pi_1 %s \\pi_2$ using a test statistic $z$ that is normally distributed under the null hypothesis. Based on your sample, you computed $z_{calc} = %4.2f$. %s", 
                                        c("\\geq", "=", "\\leq")[2+Q$direction], dZ, sRegion))
  Text= c(
    Text, 
    ColorBold("What is your final conclusion of your 5-step testing plan approach? Choose all correct answers.")
  )
  # answers
  Answers= matrix(tex2math(c(
    2, "you *reject* the null hypothesis of equal proportions as the p-value is *smaller* than $\\alpha$",
    0, "you do *not reject* the null hypothesis of equal proportions as the p-value *smaller* than $\\alpha$",
    0, "you *accept* the null hypothesis of equal proportions as the p-value is *smaller* than $\\alpha$",
    0, "you *reject* the null hypothesis of equal proportions as the p-value is *bigger* than $\\alpha$",
    1, "you do *not reject* the null hypothesis of equal proportions as the p-value *bigger* than $\\alpha$",
    0, "you *accept* the null hypothesis of equal proportions as the p-value is *bigger* than $\\alpha$",
    2, "you *reject* the null hypothesis of equal proportions as the z-statistic lies *inside* the critical region",
    0, "you do *not reject* the null hypothesis of equal proportions z-statistic lies *inside* the critical region",
    0, "you *accept* the null hypothesis of equal proportions as the z-statistic lies *inside* the critical region",
    0, "you *reject* the null hypothesis of equal proportions as the z-statistic lies *outside* the critical region",
    1, "you do *not reject* the null hypothesis of equal proportions z-statistic lies *outside* the critical region",
    0, "you *accept* the null hypothesis of equal proportions as the z-statistic lies *outside* the critical region"
  )), nrow=2)

  # correct
  Answers= answer_select(Answers[2,], (as.integer(Answers[1,])==1+as.integer(reject)), iAlt=6, mincorrect = 0, alphorder=TRUE)
  
  # return
  return(list(type=qtype, text=Text, answer=Answers$answer, correct=Answers$correct))  
}


