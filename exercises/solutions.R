library(cards)
library(dplyr)
library(tidyr)

# Import data -----------------------------------
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

# Exercise 1 ------------------------------------
# Compute the continuous summaries for AGE, BMI, HEIGHT, WEIGHT by TRT01A
ard_continuous(
  data = adsl,
  by = TRT01A,
  variables = c(AGE, BMI, HEIGHT, WEIGHT)
)


# Exercise 2 ------------------------------------
# Next, compute the categorical summaries for AGEGR1, SEX, RACE, ETHNIC by TRT01A
ard_categorical(
  data = adsl,
  by = TRT01A,
  variables = c(AGEGR1, SEX, RACE, ETHNIC)
)

# Exercise 3 ------------------------------------
# Perform all of the summaries in a single `ard_stack()` call, including:
#   - summaries by TRT01A as performed above
#   - continuous summaries from part A for AGE, BMI, HEIGHT, and WEIGHT
#   - categorical summaries from part B for AGEGR1, SEX, RACE, ETHNIC
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = c(AGE, BMI, HEIGHT, WEIGHT)),
  ard_categorical(variables = c(AGEGR1, SEX, RACE, ETHNIC))
)


# Exercise 4 ------------------------------------
# Also add the following pieces
#   - overall summaries for all of the variables
#   - total N 
ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = c(AGE, BMI, HEIGHT, WEIGHT)),
  ard_categorical(variables = c(AGEGR1, SEX, RACE, ETHNIC)), 
  .overall = TRUE,
  .total_n = TRUE
)

# Exercise 5 ------------------------------------
# Within every combination of treatment group (TRT01A) and severity (AESEV),
#   calculate the number and percentage of *unique* subjects (USUBJID) with at least one AE:
#   (1) Overall
#   (2) By each SOC (AESOC)
#   (3) By each Preferred term (AEDECOD) within SOC (AESOC)
adae <- ADAE |> 
  dplyr::filter(AESOC %in% unique(AESOC)[1:3]) |> 
  dplyr::group_by(AESOC) |> 
  dplyr::filter(AEDECOD %in% unique(AEDECOD)[1:3]) |> 
  dplyr::ungroup()

ard_stack_hierarchical(
  data = adae,
  variables = c(AESOC, AEDECOD),
  by = c(TRTA, AESEV), 
  id = USUBJID,
  denominator = adsl,
  over_variables = TRUE
)