# Introductory text and Integrity question
Q_Integrity <- function(extratime = FALSE) {

  sText <- write_in_wrapper(write_in_wrapper("Welcome to the BSTAT written re-exam.", "big"), "strong")
  if (extratime == FALSE) sText <- c(sText, paste("You are looking at the version for", ColorBold("NORMAL TIME"), "students (so 120 + 5 minutes for the selfie). If you are", ColorBold("entitled to extra time"), "then close the current quiz now and go to the extra time exam quiz."))
  else sText <- c(sText, paste("You are looking at the version for", ColorBold("EXTRA TIME"), "students (so 150 + 5 minutes for the selfie). If you are", ColorBold("not entitled to extra time"), "then close the current quiz now and go to the normal time exam quiz. Otherwise your exam results might be invalid."))
  sText <- c(sText, "Before proceeding, choose one of the statements below that applies to you.")

  sType <- "mc"
  sq <- "Introduction and statement"
  vAnswers <- c(
    "I will use the help of some friends during this exam.",
    "I will use the help of people from the internet during this exam.",
    "I will make this exam by myself without any outside help (including whatsapp) for the entire duration of the exam."
  )
  vOrder <- sample(1:length(vAnswers), length(vAnswers))
  vCorrect <- c(FALSE, FALSE, TRUE)[vOrder]
  vAnswers <- vAnswers[vOrder]

  return (list(type=sType, q=sq, text=sText, answer=vAnswers, correct=vCorrect))
}


############################################################################
############################################################################
############################################################################


# Introductory text and Integrity question
Q_IntegrityExit <- function() {

  sText <- "Before submitting your exam, choose one of the statements below that applies to you."

  sType <- "mc"
  sq <- "Exit statement"
  vAnswers <- c(
    "I used the help of some friends during this exam.",
    "I used the help of people from the internet during this exam.",
    "I made this exam by myself without any outside help (including whatsapp) for the entire duration of the exam."
  )
  vOrder <- sample(1:length(vAnswers), length(vAnswers))
  vCorrect <- c(FALSE, FALSE, TRUE)[vOrder]
  vAnswers <- vAnswers[vOrder]

  return (list(type=sType, q=sq, text=sText, answer=vAnswers, correct=vCorrect))
}


############################################################################
############################################################################
############################################################################


Q_Selfie <- function() {

  Selfie <- matrix(c(
    "your hand holds the ID card index finger at top, thumb at bottom, no other fingers.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2a.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2b.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger and pinky up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2c.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and pinky.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2d.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb (left/right or right/left).",
    "https://personal.vu.nl/a.lucas/garbage/aapje2e.jpg",
    1200, 638,
    #
    "your hand holds the ID card index finger and ring finger up, rest of fingers on the card, thumb at bottom.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2f.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb at one of the UPPER two corners.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2g.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb at one of the LOWER two corners.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2h.jpg",
    1200, 638,
    #
    "your hand holds the ID card between index finger and thumb across one of the two diagonals.",
    "https://personal.vu.nl/a.lucas/garbage/aapje2i.jpg",
    1200, 638
  ), ncol = 4, byrow = TRUE)
  i1 <- sample(1:nrow(Selfie), 1)

  sType <- "upl"
  sq <- "Selfie"
  sText <- c("Upload a selfie such that:",
             write_as_html_ul(c(
               "your face is clearly visible",
               "your ID with photo is clearly visible",
               Selfie[i1,1])),
             write_in_wrapper(
               "", "img", 
               s_wrappertag = sprintf("src=\"%s\" alt=\"\" width=\"%s\" height=\"%s\" data-decorative=\"true\"",
                                      Selfie[i1,2], Selfie[i1,3], Selfie[i1,4])
             ))
  
  return (list(type=sType, q=sq, text=sText))
}
