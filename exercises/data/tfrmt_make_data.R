library(cards)
library(tfrmt)

# demog data
adsl <- pharmaverseadam::adsl |>
  dplyr::filter(SAFFL == "Y") |> 
  dplyr::rename(
    `Age (years)` = AGE,
    `Age Group` = AGEGR1
  ) |> 
  dplyr::mutate(
    TRT01A = ifelse(!TRT01A == "Placebo", "Active", TRT01A)
  )
ard_demog <- ard_stack(
  data = adsl,
  .by = TRT01A,
  ard_continuous( variables = `Age (years)`,
                  statistic = ~ continuous_summary_fns(c("median", "p25", "p75"))),
  ard_categorical(variables = `Age Group`)
) |>
  shuffle_ard() |>
  dplyr::mutate(stat_name = ifelse(variable=="TRT01A" & stat_name=="n", "bigN", stat_name),
                ord1 = as.numeric(factor(variable, levels = c("Age (years)", "Age Group"))),
                ord2 = as.numeric(factor(label, levels = c("18-64",">64"))))|>
  dplyr::filter(stat_name=="bigN" |
                  (! variable == "TRT01A" & ! stat_name=="N")) |> 
  dplyr::select(-context) |> 
  dplyr::mutate(stat = ifelse(stat_name=="p", stat*100, stat),
                label = dplyr::case_when(
                  stat_name %in% c("p25","p75") ~ "Q1, Q3", 
                  TRUE ~ label
                ))

saveRDS(ard_demog, "exercises/data/ard_demog.rds")

# tfrmt 
tfrmt_demog <- tfrmt(
  group = variable, 
  label = label, 
  column = TRT01A,
  param = stat_name,
  value = stat, 
  sorting_cols = c(ord1, ord2) ,
  big_n = big_n_structure(
    param_val = "bigN",
    n_frmt = frmt("<br>N = xx")
  ),
  col_plan = col_plan(
    variable, 
    label,
    Placebo,
    Active,
    - starts_with("ord")
  ),
  body_plan = body_plan(
    frmt_structure(
      group_val = ".default", 
      label_val = ".default",
      frmt_combine("{n} ({p}%)",
                   n = frmt("xx"),
                   p = frmt("xx.x")
                   
      )
    ),
    frmt_structure(
      group_val = ".default", 
      label_val = "Median",
      frmt("xx.x")
    ),
    
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt_combine(
        expression = "{p25}, {p75}",                
        p25 = frmt("xx.x"),                     
        p75 = frmt("xx.x")                      
      )
    )
    ),
    col_style_plan = col_style_plan(
      col_style_structure(
        col = c("Placebo", 
                "Active"),
        align = " "
      )
    ),
    footnote_plan = footnote_plan(
      footnote_structure(
        "Pooled High and Low Dose",
        column_val = "Active"
      )
    ),
    title = "Demographic Table",
    subtitle = "Safety Population"
  )

tfrmt_to_json(tfrmt_demog, "exercises/data/tfrmt_demog.json")
