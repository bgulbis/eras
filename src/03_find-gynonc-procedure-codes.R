library(tidyverse)
library(readxl)
library(edwr)

dir_raw <- "data/raw/gynonc"

# surgeries <- read_data(dir_raw, "patients-surgeon") %>%
#     distinct() %>%
#     filter(`Primary Procedure Indicator` == "1") %>%
#     arrange(`Primary Surgeon`, `Procedure Start Date/Time`)

gynonc <- read_excel("data/external/exploratory-patient-list_gynonc.xlsx", col_types = "text")

gynonc_fin <- concat_encounters(gynonc$fin)

# run EDW:
#   * Identifiers

gynonc_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

gynonc_pie <- concat_encounters(gynonc_id$pie.id)

# run EDW:
#   * Procedures (ICD-9-CM/ICD-10-PCS) - All

gynonc_proc <- read_data(dir_raw, "^procedures") %>%
    as.procedures() %>%
    filter(proc.source == "ICD-10-CM")

procs <- count(gynonc_proc, proc.code) %>%
    arrange(desc(n))

proc_codes <- concat_encounters(gynonc_proc$proc.code)

# run EDW:
#   * Lookup - Procedure Descriptions

proc_desc <- read_data(dir_raw, "lookup") %>%
    rename(proc.code = `Procedure Code`,
           proc.desc = `Procedure Code Description`) %>%
    distinct() %>%
    left_join(procs, by = "proc.code") %>%
    arrange(desc(n))

gynonc_proc_desc <- left_join(gynonc_proc, proc_desc, by = "proc.code")

# x <- filter(gynonc_proc, proc.code %in% c("0UT90ZZ", "0UTC0ZZ", "0UT70ZZ",
#                                           "0UT20ZZ", "0DBS0ZX", "0DBS0ZZ",
#                                           "0UT00ZZ", "0UT10ZZ", "0UT60ZZ",
#                                           "0UT50ZZ", "0UB20ZZ", "0UDB8ZZ",
#                                           "0UB98ZZ", "0UBM0ZZ", "0UT54ZZ",
#                                           "0UB04ZZ", "0DBW0ZX")) %>%
#     distinct(pie.id)
#
# y <- anti_join(gynonc_id, gynonc_proc, by = "pie.id")
#
# proc_mid <- concat_encounters(y$millennium.id)

# y <- anti_join(gynonc_proc_desc, x, by = "pie.id")
# z <- distinct(gynonc_proc, pie.id)

# probably gynonc procedure codes: 0DB64Z3, 0DB64ZZ, 0D164ZA

# edw patient list -------------------------------------

# run EDW:
#   * Patients - by Procedure Code
#       - Procedure Code: 0UT90ZZ;0UTC0ZZ;0UT70ZZ;0UT20ZZ;0DBS0ZX;0DBS0ZZ;0UT00ZZ;0UT10ZZ;0UT60ZZ;0UT50ZZ;0UB20ZZ;0UDB8ZZ;0UB98ZZ;0UBM0ZZ;0UT54ZZ;0UB04ZZ;0DBW0ZX
#       - Physician- Procedure: LucciIII, Joseph Anthony MD;Nugent, Elizabeth Kathleen MD
#       - Admit date: 1/1/2016 - 7/1/2016

gynonc_pts <- read_data(dir_raw, "patients_eras-gynonc") %>%
    as.patients()

write_rds(gynonc_pts, "data/tidy/gynonc_pts.Rds", "gz")

gynonc_pie <- concat_encounters(gynonc_pts$pie.id)

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

gynonc_id2 <- read_data(dir_raw, "id_eras") %>%
    as.id()

#   * Encounters

