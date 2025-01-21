# ARD Exercise: Demographic summary table using {cards}+{gtsummary}

# For this exercise, create a summary table using `tbl_ard_summary()`
#   1. Split summary statistics by TRT01A
#   2. Include AGE, SEX, and RACE. For AGE, display the "mean (sd)" and "median (p25, p75)" on separate rows
#      HINT: Use `type = AGE ~ 'continuous2'` and `statistic = AGE ~ c("{mean} ({sd})", "{median} ({p25}, {p75})")`
#   3. Include a column of overall results using `tbl_ard_summary(overall=TRUE)`

# Setup
## Import data
library(cards)
library(gtsummary)
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")

# Create the ARD with the needed statistics
ard <- 
  ard_stack(
    adsl,
    .by = TRT01A,
    ard_continuous(variables = AGE),
    ard_categorical(variables = c(SEX, RACE)),
    .attributes = TRUE, 
    .overall = TRUE
  )

# Create table
ard |> 
  tbl_ard_summary(
    by = ,
    include = ,
    type = ,
    statistic = ,
    overall = 
  )
