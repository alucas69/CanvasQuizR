# CanvasQuizR
CanvasQuizR generates Canvas quiz questions (with variations for each question) based on a set of R functions.

## Setup
### 1. Install packages
Install all the dependencies. If you have issues with installing the `car` package, then make sure that `gcc` is installed.

### 2. Set working directory
First, run `getwd()` in R console to get your current working directory. Then, use `setwd()` to change your working directory to the `2020resitv_1` subdirectory of this project.  

For example, if the output of `getwd()` is `"/Users/andre/Developer/CanvasQuizR/"`, then you should run `setwd("/Users/andre/Developer/CanvasQuizR/2020resitv_1")`

### 3. Set emergency contact
Make sure to edit the `emergency_message` in `2020_resitv_1/Exam20200629.R` such that it matches the emergency contact.

## Running
To run CanvasQuizR, run the `2020resitv_1/Exam20200629.R` entrypoint. This will generate a HTML file in the `build/` directory.