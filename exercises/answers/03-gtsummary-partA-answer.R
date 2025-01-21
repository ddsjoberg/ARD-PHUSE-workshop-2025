# ARD Exercise: Demographic summary table using {gtsummary}

# For this exercise, create a summary table using `tbl_summary()`
#   1. Split summary statistics by TRT01A
#   2. Include AGE, SEX, and RACE. For AGE, display the "mean (sd)" and "median (p25, p75)" on separate rows
#      HINT: Use `type = AGE ~ 'continuous2'` and `statistic = AGE ~ c("{mean} ({sd})", "{median} ({p25}, {p75})")`
#   3. Include a column of overall results using `add_overall()`
#   BONUS: Use `cards::gather_ard()` to extract the ARD from the table object

# Setup
## Import data
library(gtsummary)
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")

# Create table
tbl <- adsl |> 
  tbl_summary(
    by = TRT01A,
    include = c(AGE, SEX, RACE),
    type = AGE ~ 'continuous2',
    statistic = AGE ~ c("{mean} ({sd})", "{median} ({p25}, {p75})")
  ) |> 
  add_overall()
tbl

# Extract ARD from tbl object
gather_ard(tbl)
