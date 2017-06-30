library(tidyverse)
library(readxl)
library(edwr)

# run EDW:
#   * Patients - by Procedure Code
#       - Procedure Code: 0DB64Z3;0DB64ZZ;0D164ZA
#       - Admit date: 1/1/2016 - 7/1/2016

bari_pts <- read_data("data/raw", "patients_eras-bari") %>%
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

bari_id <- read_data("data/raw", "id_eras") %>%
    as.id()

bari_person <- concat_encounters(bari_id$person.id)

# run EDW query:
#   * Encounters - by Person ID

bari_encounters <- read_data("data/raw", "encounters") %>%
    as.encounters() %>%
    filter(visit.type %in% c("Inpatient", "Emergency", "Bedded Outpatient", "Observation")) %>%
    group_by(person.id) %>%
    arrange(admit.datetime, .by_group = TRUE) %>%
    left_join(bari_pts[c("pie.id", "discharge.datetime")], by = "pie.id") %>%
    fill(discharge.datetime) %>%
    filter(!is.na(discharge.datetime)) %>%
    mutate(revisit_days = difftime(admit.datetime, discharge.datetime, units = "days")) %>%
    filter(revisit_days > 0,
           revisit_days <= 30)

bari_revisit <- distinct(bari_encounters, person.id, .keep_all = TRUE)

bari_locations <- read_data("data/raw", "locations") %>%
    as.locations() %>%
    tidy_data()
