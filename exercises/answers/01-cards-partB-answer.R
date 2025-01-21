# Exercise: Adverse Events summaries using {cards}

# Load necessary packages
library(cards) 

# Import data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
adae <- pharmaverseadam::adae 

## A.  Calculate the number and percentage of *unique* subjects (USUBJID) with at least one AE:
#  - Overall
#  - By each SOC (AESOC)
#  - By each Preferred term (AEDECOD) within SOC (AESOC)
# By every combination of treatment group (TRT01A) and severity (AESEV)

ard_stack_hierarchical(
  data = adae,
  variables = c(AESOC, AEDECOD),
  by = c(TRT01A, AESEV), 
  id = USUBJID,
  denominator = adsl,
  over_variables = TRUE
)
 