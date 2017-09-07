library(tidyverse)
library(readxl)
library(edwr)

dir_raw <- "data/raw/bari"

surgeries <- read_data(dir_raw, "patients-surgeon") %>%
    distinct() %>%
    # filter(`Primary Procedure Indicator` == "1") %>%
    arrange(`Primary Surgeon`, `Procedure Start Date/Time`)

bari <- read_excel("data/external/exploratory-patient-list_bariatrics.xlsx", col_types = "text")

bari_fin <- concat_encounters(bari$fin)

# run EDW:
#   * Identifiers

bari_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

bari_pie <- concat_encounters(bari_id$pie.id)

# run EDW:
#   * Procedures (ICD-9-CM/ICD-10-PCS) - All

bari_proc <- read_data(dir_raw, "^procedures") %>%
    as.procedures() %>%
    filter(proc.source == "ICD-10-CM")

procs <- count(bari_proc, proc.code) %>%
    arrange(desc(n))

proc_codes <- concat_encounters(bari_proc$proc.code)

# run EDW:
#   * Lookup - Procedure Descriptions

proc_desc <- read_data(dir_raw, "lookup") %>%
    rename(proc.code = `Procedure Code`,
           proc.desc = `Procedure Code Description`) %>%
    distinct() %>%
    left_join(procs, by = "proc.code") %>%
    arrange(desc(n))

bari_proc_desc <- left_join(bari_proc, proc_desc, by = "proc.code")

# x <- filter(bari_proc, (proc.code == "0DB64Z3" | proc.code == "0D164ZA")) %>%
#     distinct(pie.id)
#
# y <- anti_join(bari_proc_desc, x, by = "pie.id")

# probably bari procedure codes: 0DB64Z3, 0DB64ZZ, 0D164ZA

# edw patient list -------------------------------------

# run EDW:
#   * Patients - by Procedure Code
#       - Procedure Code: 0DB64Z3;0DB64ZZ;0D164ZA
#       - Admit date: 1/1/2016 - 7/1/2016

bari_pts <- read_data(dir_raw, "patients_eras-bari") %>%
    as.patients()

write_rds(bari_pts, "data/tidy/bari_pts.Rds", "gz")

bari_pie <- concat_encounters(bari_pts$pie.id)

# run EDW queries:
#   * Clinical Events
#       - Clinical Event: Pain Intensity NRS (0-10); Pain Intensity; Primary
#       Pain Intensity; Pain Intensity General; Pain Intensity Secondary
#   * Demographics
#   * DRG
#   * Identifiers
#   * Location History
#   * Medications
#   * Visit Data

bari_id2 <- read_data(dir_raw, "id_eras") %>%
    as.id()

#   * Encounters

