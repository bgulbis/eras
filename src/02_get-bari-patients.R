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
#   * Medications - Inpatient Continuous - All
#   * Medications - Inpatient Intermittent - All
#   * Surgery Times
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

# bari_revisit <- distinct(bari_encounters, person.id, .keep_all = TRUE)

bari_revisit_pie <- concat_encounters(bari_encounters$pie.id)

# run EDW query:
#   * Visit Data
bari_revisit <- read_data("data/raw", "revisit") %>%
    as.visits()



bari_locations <- read_data("data/raw", "locations") %>%
    as.locations() %>%
    tidy_data()

bari_surg_times <- read_data("data/raw", "surgery-times") %>%
    rename(pie.id = `PowerInsight Encounter Id`,
           surgery_start = `Start Date/Time`,
           surgery_stop = `Stop Date/Time`,
           room_in = `Patient In Room Date/Time`,
           room_out = `Patient Out Room Date/Time`,
           recovery_in = `Patient In Recovery Date/Time`,
           recovery_out = `Patient Out Recovery Date/Time`) %>%
    distinct() %>%
    filter(!is.na(surgery_start))

bari_visit <- read_data("data/raw", "visit") %>%
    as.visits()
# remove those that aren't preadmit?

bari_floor <- bari_locations %>%
    left_join(bari_surg_times, by = "pie.id") %>%
    filter(location == "Jones 9 Bariatric/General Surgery",
           unit.count == 2) %>%
    mutate(recovery_duration = difftime(arrive.datetime, room_out, units = "min")) %>%
    filter(recovery_duration > 0)

bari_readmit <- bari_visit %>%
    inner_join(bari_revisit, by = "pie.id")

# pain meds --------------------------------------------

opiods <- tibble(name = c("narcotic analgesics", "narcotic analgesic combinations"),
                 type = "class",
                 group = "sched")

cont_opiods <- tibble(name = c("fentanyl", "morphine", "hydromorphone", "mepiridine", "remifentanyl", "sufentanyl"),
                      type = "med",
                      group = "cont")

meds_sched <- read_data("data/raw", "meds-sched") %>%
    as.meds_sched()

meds_cont <- read_data("data/raw", "meds-cont") %>%
    as.meds_cont() %>%
    tidy_data(cont_opiods, meds_sched)

meds_pain <- tidy_data(meds_sched, opiods)
