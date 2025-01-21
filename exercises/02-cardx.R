# ARD Exercise: Demographic comparison with {cardx}

# In a previous exercise, you calculated summary deomgraphic statistics by treament arm
# Here, we will use a subset of those variables, and compare their values across treatment arms
# Variables: AGE, SEX, RACE
# Treatment variable: TRT01A

# Compare AGE with the Kruskal-Wallis test and SEX and AGE with Pearson's Chi-square test
# DISCLAIMER: Don't compare baseline characteristics in randomized trials in real life!


# Setup

## Import data
adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")

##  A. First, compute the Kruskal-Wallis test for AGE by TRT01A
kruskal_ard <- 
  cardx::ard_stats_kruskal_test(
    data = , 
    by = ,
    variables = 
  )

##  B. First, compute the Chi-squared test for SEX, RACE by TRT01A
chisq_ard <- 
  cardx::ard_stats_chisq_test(
    data = , 
    by = ,
    variables = 
  )
  
##  C. Combine your results with `cards::bind_ard()` and subset the ARD to include the rows with p-values only
final_ard <- 
  cards::bind_ard(

  ) |> 
  dplyr::filter()

##  Bonus. Which test returned a warning? If any, print the warning. 
##         Note that the warning column is a list-column which requires special handling



