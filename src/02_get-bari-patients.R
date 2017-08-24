library(tidyverse)
library(readxl)
library(lubridate)
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

bari_mbo <- concat_encounters(bari_id$millennium.id)

# run MBO query:
#   * Pain PCA Pump
#   * Pain Scores

bari_person <- concat_encounters(bari_id$person.id)

# run EDW query:
#   * Encounters - by Person ID

# revisits ---------------------------------------------

# exclude bedded outpatient (probably planned)
bari_encounters <- read_data("data/raw", "encounters") %>%
    as.encounters() %>%
    filter(visit.type %in% c("Inpatient", "Emergency", "Observation")) %>%
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

# revisit admit type and source
bari_revisit <- read_data("data/raw", "revisit") %>%
    as.visits() %>%
    semi_join(bari_encounters, by = "pie.id")

# remove those that aren't preadmit
bari_visit <- read_data("data/raw", "^visit") %>%
    as.visits() %>%
    filter(admit.type == "Preadmit Not OB")

# surgery times ----------------------------------------

bari_locations <- read_data("data/raw", "locations") %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(bari_visit, by = "pie.id")

bari_surg_times <- read_data("data/raw", "surgery-times") %>%
    rename(pie.id = `PowerInsight Encounter Id`,
           surgery_start = `Start Date/Time`,
           surgery_stop = `Stop Date/Time`,
           room_in = `Patient In Room Date/Time`,
           room_out = `Patient Out Room Date/Time`,
           recovery_in = `Patient In Recovery Date/Time`,
           recovery_out = `Patient Out Recovery Date/Time`) %>%
    distinct() %>%
    semi_join(bari_visit, by = "pie.id") %>%
    filter(!is.na(surgery_start)) %>%
    mutate_at(c("surgery_start", "surgery_stop", "room_in", "room_out"),
              ymd_hms, tz = "US/Central") %>%
    arrange(pie.id, surgery_start) %>%
    add_count(pie.id)

bari_floor <- bari_locations %>%
    semi_join(bari_visit, by = "pie.id") %>%
    left_join(bari_surg_times, by = "pie.id") %>%
    filter(!is.na(location)) %>%
    mutate(recovery_duration = difftime(arrive.datetime, room_out, units = "min")) %>%
    filter(recovery_duration > 0) %>%
    group_by(pie.id) %>%
    arrange(recovery_duration, .by_group = TRUE) %>%
    distinct(pie.id, .keep_all = TRUE) %>%
    filter(location == "Jones 9 Bariatric/General Surgery")

# bari_readmit <- bari_visit %>%
#     inner_join(bari_revisit, by = "pie.id")

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

meds_pain <- tidy_data(meds_sched, opiods) %>%
    left_join(bari_floor[c("pie.id", "room_out", "arrive.datetime")], by = "pie.id") %>%
    mutate(timing = case_when(med.datetime < room_out ~ "or",
                              med.datetime < arrive.datetime ~ "pacu",
                              TRUE ~ "floor"))

    filter(med.datetime >= room_out,
           med.datetime <= arrive.datetime)

meds_pain_floor <- meds_pain %>%
    left_join(bari_floor[c("pie.id", "room_out", "arrive.datetime")], by = "pie.id") %>%
    filter(med.datetime > arrive.datetime)

meds_pain_or <- meds_pain %>%
    left_join(bari_floor[c("pie.id", "room_out", "arrive.datetime")], by = "pie.id") %>%
    filter(med.datetime < room_out)

meds_pain_cont <- meds_cont %>%
    calc_runtime() %>%
    summarize_data()

# pain scores ------------------------------------------

pain_scores <- read_data("data/raw", "pain-scores", FALSE) %>%
    as.pain_scores()

pain_pca <- read_data("data/raw", "pain-pca", FALSE) %>%
    as.pain_scores()

