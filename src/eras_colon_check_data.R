library(tidyverse)
library(readxl)

pts <- read_excel(
    "data/external/eras_colon_patients.xlsx", 
    col_names = c("fin", "surgery_date"), 
    col_types = c("numeric", "date"), 
    skip = 1
)

fin <- edwr::concat_encounters(pts$fin)
print(fin)
