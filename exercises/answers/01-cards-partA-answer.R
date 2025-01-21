# ARD Exercise: Demographic summaries using {cards}

# For this exercise, compute all the necessary summaries for a demographics table that includes age, age group, sex, race, ethnicity, bmi, height, and weight. We want to compute these summaries within each treatment group (High Dose, Low Dose, Placebo) and overall.

# Setup
## Load necessary packages
library(cards)
library(dplyr)
library(tidyr)

## Import data
adsl <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") |>
  left_join(
    pharmaverseadam::advs |>
      filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      group_by(USUBJID, PARAMCD) |>
      arrange(ADY) |>
      slice(1) |>
      ungroup() |>
      select(USUBJID, PARAMCD, AVAL) |>
      pivot_wider(names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  )

##  A. First, compute the continuous summaries for AGE, BMI, HEIGHT, WEIGHT by TRT01A

ard_continuous(
  data = adsl,
  by = TRT01A,
  variables = c(AGE, BMI, HEIGHT, WEIGHT)
)


##  B. Next, compute the categorical summaries for AGEGR1, SEX, RACE, ETHNIC by TRT01A

ard_categorical(
  data = adsl,
  by = TRT01A,
  variables = c(AGEGR1, SEX, RACE, ETHNIC)
)

## C. Perform all of the summaries in a single ard_stack() call, including:
#   - summaries by TRT01A as performed above
#      - continuous summaries from part A for AGE, BMI, HEIGHT, and WEIGHT
#      - categorical summaries from part B for AGEGR1, SEX, RACE, ETHNIC
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = c(AGE, BMI, HEIGHT, WEIGHT)),
  ard_categorical(variables = c(AGEGR1, SEX, RACE, ETHNIC))
)


## D. BONUS!
#   For part C. above, add the following pieces
#   - overall summaries for all of the variables
#   - total N
# (Hint: Modify the `.overall` and `.total_n` arguments, respectively)
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = c(AGE, BMI, HEIGHT, WEIGHT)),
  ard_categorical(variables = c(AGEGR1, SEX, RACE, ETHNIC)), 
  .overall = TRUE,
  .total_n = TRUE
)

