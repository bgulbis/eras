library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(edwr)

dir_raw <- "data/raw/bari_curr"

# run EDW:
#   * Patients - by Procedure Code
#       - Procedure Code: 0DB64Z3;0DB64ZZ;0D164ZA
#       - Admit date: 1/1/2017 - 10/1/2017

bari_pts <- read_data(dir_raw, "patients_eras-bari") %>%
    as.patients()

write_rds(bari_pts, "data/tidy/bari_pts_current.Rds", "gz")

bari_pie <- concat_encounters(bari_pts$pie.id)

# run EDW queries:
#   * Demographics
#   * DRG Codes - All
#   * Identifiers - by PowerInsight Encounter Id
#   * Location History
#   * Medications - Inpatient Continuous - All
#   * Medications - Inpatient Intermittent - All
#   * Surgeon - by Patient
#   * Surgery Times
#   * Visit Data

bari_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

bari_mbo <- concat_encounters(bari_id$millennium.id)

# run MBO query:
#   * Pain PCA Pump
#   * Pain Scores

bari_person <- concat_encounters(bari_id$person.id)

# run EDW query:
#   * Encounters - by Person ID

# remove those that aren't preadmit
bari_visit <- read_data(dir_raw, "^visit") %>%
    as.visits() %>%
    filter(admit.type == "Preadmit Not OB")

bari_surgeon <- read_data(dir_raw, "surgeon") %>%
    distinct() %>%
    rename(pie.id = `PowerInsight Encounter Id`,
           start.datetime = `Start Date/Time`,
           surgeon = `Primary Surgeon`) %>%
    mutate_at("start.datetime", ymd_hms, tz = "US/Central") %>%
    filter(surgeon %in% c("Wilson, Erik Browning MD",
                          "SnyderSr, Brad Elliot MD",
                          "Wilson, Todd David MD",
                          "Mehta, Sheilendra Suresh MD",
                          "Bajwa, Kulvinder S MD"))

# surgery times ----------------------------------------

bari_locations <- read_data(dir_raw, "locations") %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(bari_visit, by = "pie.id")

bari_surg_times <- read_data(dir_raw, "surgery-times") %>%
    # rename(pie.id = `PowerInsight Encounter Id`,
    #        surgery_start = `Start Date/Time`,
    #        surgery_stop = `Stop Date/Time`,
    #        room_in = `Patient In Room Date/Time`,
    #        room_out = `Patient Out Room Date/Time`,
    #        recovery_in = `Patient In Recovery Date/Time`,
    #        recovery_out = `Patient Out Recovery Date/Time`) %>%
    # distinct() %>%
    as.surgery_times() %>%
    semi_join(bari_visit, by = "pie.id") %>%
    filter(!is.na(surgery_start)) %>%
    mutate_at(c("surgery_start", "surgery_stop", "room_in", "room_out"),
              ymd_hms, tz = "US/Central") %>%
    arrange(pie.id, surgery_start) %>%
    add_count(pie.id)

# bari_floor <- bari_locations %>%
#     semi_join(bari_visit, by = "pie.id") %>%
#     left_join(bari_surg_times, by = "pie.id") %>%
#     filter(!is.na(location)) %>%
#     mutate(pacu_hours = difftime(arrive.datetime, room_out, units = "hours"),
#            or_hours = difftime(room_out, room_in, units = "hours")) %>%
#     filter(pacu_hours > 0) %>%
#     group_by(pie.id) %>%
#     arrange(pacu_hours, .by_group = TRUE) %>%
#     distinct(pie.id, .keep_all = TRUE) %>%
#     filter(location == "Jones 9 Bariatric/General Surgery")

bari_floor <- bari_locations %>%
    semi_join(bari_visit, by = "pie.id") %>%
    left_join(bari_surg_times, by = "pie.id") %>%
    left_join(bari_visit[c("pie.id", "admit.datetime", "discharge.datetime")], by = "pie.id") %>%
    group_by(pie.id) %>%
    filter(arrive.datetime > surgery_stop,
           location != "Hermann 1 Virtual Emergency Dept") %>%
    arrange(arrive.datetime, .by_group = TRUE) %>%
    distinct(pie.id, .keep_all = TRUE) %>%
    filter(location == "Jones 9 Bariatric/General Surgery") %>%
    mutate(pacu_hours = difftime(arrive.datetime, room_out, units = "hours"),
           or_hours = difftime(room_out, room_in, units = "hours"),
           preop_los = difftime(surgery_start, admit.datetime, units = "days"),
           postop_los = difftime(discharge.datetime, surgery_stop, units = "days")) %>%
    filter(pacu_hours > 0)

data_patients <- bari_floor %>%
    select(pie.id, or_hours, pacu_hours, arrive.datetime:room_out) %>%
    rename(floor_days = unit.length.stay) %>%
    semi_join(bari_surgeon, by = "pie.id") %>%
    mutate(group = "current")

bari_id <- semi_join(bari_id, data_patients, by = "pie.id")
bari_pts <- semi_join(bari_pts, data_patients, by = "pie.id")

# demographics -----------------------------------------

data_demographics <- read_data(dir_raw, "demographics") %>%
    as.demographics() %>%
    semi_join(data_patients, by = "pie.id")


# revisits ---------------------------------------------

# exclude bedded outpatient (probably planned)
bari_encounters <- read_data(dir_raw, "encounters") %>%
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
data_revisit <- read_data(dir_raw, "revisit") %>%
    as.visits() %>%
    inner_join(bari_encounters[c("pie.id", "revisit_days")], by = "pie.id") %>%
    select(-(arrival.datetime:discharge.datetime))

# pain meds --------------------------------------------

opiods <- tibble(name = c("narcotic analgesics",
                          "narcotic analgesic combinations",
                          "acetaminophen"),
                 type = c(rep("class", 2), "med"),
                 group = "sched")

cont_opiods <- tibble(name = c("fentanyl",
                               "morphine",
                               "hydromorphone",
                               "mepiridine",
                               "remifentanyl",
                               "sufentanyl"),
                      type = "med",
                      group = "cont")

meds_sched <- read_data(dir_raw, "meds-sched") %>%
    as.meds_sched()

meds_pain <- tidy_data(meds_sched, opiods) %>%
    inner_join(data_patients[c("pie.id", "room_out", "arrive.datetime")], by = "pie.id") %>%
    mutate(timing = case_when(med.datetime < room_out ~ "or",
                              med.datetime < arrive.datetime ~ "pacu",
                              TRUE ~ "floor"),
           time_surg = difftime(med.datetime, room_out, units = "hours"))

data_pain_meds <- meds_pain %>%
    filter(time_surg < 24) %>%
    group_by(pie.id, med, med.dose.units, route) %>%
    # mutate(postop_day = difftime(floor_date(med.datetime, "day"),
    #                              floor_date(room_out, "day"),
    #                              units = "days")) %>%
    # group_by(pie.id, postop_day, med, med.dose.units, route) %>%
    summarize_at("med.dose", sum, na.rm = TRUE) %>%
    arrange(pie.id, med)

opiods <- med_lookup(c("narcotic analgesics", "narcotic analgesic combinations")) %>%
    mutate_at("med.name", str_to_lower)

data_opiods <- data_pain_meds %>%
    filter(med %in% opiods$med.name)

# seems like these are all PCA

# meds_cont <- read_data(dir_raw, "meds-cont") %>%
#     as.meds_cont() %>%
#     tidy_data(cont_opiods, meds_sched)
#
# meds_pain_cont <- meds_cont %>%
#     calc_runtime() %>%
#     summarize_data() %>%
#     semi_join(data_patients, by = "pie.id")

# pain scores ------------------------------------------

pain_scores <- read_data(dir_raw, "pain-scores", FALSE) %>%
    as.pain_scores() %>%
    semi_join(bari_id, by = "millennium.id") %>%
    inner_join(bari_id[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    left_join(data_patients, by = "pie.id") %>%
    filter(event.datetime > surgery_start) %>%
    mutate(time_surg = difftime(event.datetime, surgery_stop, units = "hours"))

pain_avg_all <- pain_scores %>%
    filter(time_surg <= 24) %>%
    rename(vital.datetime = event.datetime,
           vital = event,
           vital.result = event.result) %>%
    mutate_at("vital.result", as.numeric) %>%
    select(pie.id, everything(), -millennium.id)

pain_avg_prior <- pain_scores %>%
    filter(time_surg <= 24,
           order.id == "0") %>%
    rename(vital.datetime = event.datetime,
           vital = event,
           vital.result = event.result) %>%
    mutate_at("vital.result", as.numeric) %>%
    select(pie.id, everything(), -millennium.id)

class(pain_avg_all) <- append(class(pain_avg_all), c("vitals", "tbl_edwr"), after = 0L)
attr(pain_avg_all, "data") <- "edw"

data_pain_scores_all <- pain_avg_all %>%
    calc_runtime() %>%
    summarize_data()

class(pain_avg_prior) <- append(class(pain_avg_prior), c("vitals", "tbl_edwr"), after = 0L)
attr(pain_avg_prior, "data") <- "edw"

data_pain_scores_prior <- pain_avg_prior %>%
    calc_runtime() %>%
    summarize_data()

# pca --------------------------------------------------
pca_actions <- c("pca continuous rate dose" = "pca_rate",
                 "pca demand dose unit" = "pca_dose_unit",
                 "pca demand dose" = "pca_dose",
                 "pca doses delivered" = "pca_delivered",
                 "pca drug" = "pca_drug",
                 "pca loading dose" = "pca_load",
                 "pca lockout interval \\(minutes\\)" = "pca_lockout",
                 "pca total demands" = "pca_demands")

pain_pca <- read_data(dir_raw, "pain-pca", FALSE) %>%
    as.pain_scores() %>%
    inner_join(bari_id[c("millennium.id", "pie.id")], by = "millennium.id") %>%
    select(pie.id, millennium.id:event.result) %>%
    mutate_at("event", str_replace_all, pattern = pca_actions) %>%
    distinct() %>%
    spread(event, event.result) %>%
    mutate_at(c("pca_demands",
                "pca_dose",
                "pca_delivered",
                "pca_load",
                "pca_lockout",
                "pca_rate"),
              as.numeric) %>%
    left_join(data_patients[c("pie.id", "room_out", "depart.datetime")], by = "pie.id") %>%
    group_by(pie.id, event.datetime) %>%
    mutate(total_dose = sum(pca_delivered * pca_dose, pca_load, na.rm = TRUE),
           postop_day = difftime(floor_date(event.datetime, "day"),
                                 floor_date(room_out, "day"),
                                 units = "days"),
           time_surg = difftime(event.datetime, room_out, units = "hours"))

data_pca <- pain_pca %>%
    # group_by(pie.id, postop_day, pca_drug) %>%
    filter(time_surg < 24) %>%
    group_by(pie.id, pca_drug) %>%
    summarize_at(c("pca_demands",
                   "pca_delivered",
                   "total_dose"),
                 sum, na.rm = TRUE)

# nausea meds ------------------------------------------
# emesis
# number prn

dirr::save_rds("data/tidy/bari_current", "^data_")
