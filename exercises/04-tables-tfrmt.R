 
# Setup - Modifying a table with {tfrmtbuilder} --------------------------------

# Load libraries
library(tfrmtbuilder)
library(tfrmt)

# Import ARD and tfrmt
ard_demog <- readRDS("exercises/data/ard_demog.rds")
tfrmt_demog <- json_to_tfrmt("exercises/data/tfrmt_demog.json")

# Run app 
tfrmtbuilder(
  tfrmt = tfrmt_demog,
  data = ard_demog,
  mockmode = FALSE # to create table with values
)

# Exercise - Modifying a table with {tfrmtbuilder} -----------------------------

# 1.  Switch the order of treatment columns so Active is first
#      (hint: Column Plan)

# 2. Change the footnote marks to letters instead of numbers
#      (hint: Footnote Plan)

# 3. Split the table into 2 parts by setting a maximum of 3 rows
#      (hint: Page Plan)
 
# 4. Export table (hint: Export tab)


