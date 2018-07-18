library(tidyverse)
library(lubridate)
library(edwr)

tz <- "US/Central"

begin_month <- mdy("7/1/2017", tz = tz)
begin_abbrev <- format(begin_month, "%Y-%m")

end_month <- mdy("6/1/2018", tz = tz)
end_abbrev <- format(end_month, "%Y-%m")

dir_raw <- paste0("data/raw/gynonc/", begin_abbrev, "_", end_abbrev)

if (!dir.exists(dir_raw)) dir.create(dir_raw)

dirr::gzip_files(dir_raw)

# pull data --------------------------------------------

# run MBO:
#   * Patients - by Procedure Code
#       - Facility (Curr): HH HERMANN;HH Clinics;HH Trans Care;HH Rehab
#       - Procedure Code: 0UT90ZZ;0UTC0ZZ;0UT70ZZ;0UT20ZZ;0DBS0ZX;0DBS0ZZ;0UT00ZZ;0UT10ZZ;0UT60ZZ;0UT50ZZ;0UB20ZZ;0UDB8ZZ;0UB98ZZ;0UBM0ZZ;0UT54ZZ;0UB04ZZ;0DBW0ZX
#       - Admit date: 7/1/2017 - 7/1/2018

gynonc_pts <- read_data(dir_raw, "patients_eras-gynonc", FALSE) %>%
    as.patients()

mbo_id <- concat_encounters(gynonc_pts$millennium.id)

# run MBO queries:
#   * Demographics
#   * Location History
#   * Medications - Home and Discharge
#       - Order Type: Recorded / Home Meds
#   * Medications - Inpatient - All
#   * Pain PCA Pump
#   * Pain Scores
#   * Visit Data

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = "0") %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

# run EDW queries:
#   * Identifiers - by Millennium Encounter ID

gynonc_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

gynonc_pie <- concat_encounters(gynonc_id$pie.id)

# run EDW queries:
#   * Surgeon - by Patient
#   * Surgeries
#   * Surgery Times

gynonc_person <- concat_encounters(gynonc_id$person.id)

# run EDW query:
#   * Encounters - by Person ID

# limit patients ---------------------------------------

# remove those that aren't preadmit
gynonc_visit <- read_data(dir_raw, "^visit", FALSE) %>%
    as.visits() %>%
    filter(admit.type == "Preadmit Not OB") %>%
    left_join(
        gynonc_id[c("millennium.id", "pie.id")],
        by = "millennium.id"
    )

gynonc_surgeon <- read_data(dir_raw, "surgeon") %>%
    distinct() %>%
    rename(
        pie.id = `PowerInsight Encounter Id`,
        start.datetime = `Start Date/Time`,
        surgeon = `Primary Surgeon`
    ) %>%
    mutate_at("start.datetime", ymd_hms, tz = "US/Central") %>%
    filter(
        surgeon %in% c(
            "Nugent, Elizabeth Kathleen MD",
            "LucciIII, Joseph Anthony MD"
        )
    )

# surgery times ----------------------------------------

gynonc_locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(gynonc_visit, by = "millennium.id")

surg <- read_data(dir_raw, "surgeries") %>%
    as.surgeries() %>%
    filter(primary.proc) %>%
    select(pie.id, surgery, surg.start.datetime)

gynonc_surg_times <- read_data(dir_raw, "surgery-times") %>%
    as.surgery_times() %>%
    semi_join(gynonc_visit, by = "pie.id") %>%
    filter(!is.na(surgery_start)) %>%
    mutate_at(
        c("surgery_start", "surgery_stop", "room_in", "room_out"),
        ymd_hms, tz = "US/Central"
    ) %>%
    arrange(pie.id, surgery_start) %>%
    left_join(
        surg,
        by = c("pie.id", "surgery_start" = "surg.start.datetime")
    ) %>%
    filter(
        !(
            surgery %in% c(
                "Laparoscopy Diagnostic",
                "Hernia Hiatal Repair Laparoscopic"
            )
        )
    ) %>%
    add_count(pie.id) %>%
    distinct(pie.id, surgery_start, .keep_all = TRUE)

gynonc_floor <- gynonc_locations %>%
    inner_join(
        gynonc_visit[c(
            "millennium.id",
            "pie.id",
            "admit.datetime",
            "discharge.datetime"
        )],
        by = "millennium.id"
    ) %>%
    left_join(gynonc_surg_times, by = "pie.id") %>%
    group_by(millennium.id) %>%
    filter(
        arrive.datetime > surgery_stop,
        location != "Hermann 1 Virtual Emergency Dept"
    ) %>%
    arrange(arrive.datetime, .by_group = TRUE) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    filter(location %in% c("HH 9EJP", "HH 9WJP")) %>%
    mutate(
        pacu_hours = difftime(
            arrive.datetime,
            room_out,
            units = "hours"
        ),
        or_hours = difftime(
            room_out,
            room_in,
            units = "hours"
        ),
        preop_los = difftime(
            surgery_start,
            admit.datetime,
            units = "days"
        ),
        postop_los = difftime(
            discharge.datetime,
            surgery_stop,
            units = "days"
        ),
        dc_day = difftime(
            floor_date(discharge.datetime, unit = "day"),
            floor_date(surgery_stop, unit = "day"),
            units = "days"
        )
    ) %>%
    filter(pacu_hours > 0)

data_patients <- gynonc_floor %>%
    select(
        millennium.id,
        pie.id,
        surgery,
        dc_day,
        or_hours,
        pacu_hours,
        preop_los,
        postop_los,
        arrive.datetime:room_out
    ) %>%
    rename(floor_days = unit.length.stay) %>%
    inner_join(
        gynonc_surgeon,
        by = c("pie.id", "surgery_start" = "start.datetime")
    )

gynonc_id <- semi_join(gynonc_id, data_patients, by = "pie.id")
gynonc_pts <- semi_join(gynonc_pts, data_patients, by = "millennium.id")

# revisits ---------------------------------------------

# exclude bedded outpatient (probably planned)
gynonc_encounters <- read_data(dir_raw, "encounters") %>%
    as.encounters() %>%
    filter(
        visit.type %in% c(
            "Inpatient",
            "Emergency",
            "Observation"
        )
    ) %>%
    group_by(person.id) %>%
    arrange(admit.datetime, .by_group = TRUE) %>%
    left_join(
        gynonc_pts[c("millennium.id", "discharge.datetime")],
        by = "millennium.id"
    ) %>%
    fill(discharge.datetime) %>%
    filter(!is.na(discharge.datetime)) %>%
    mutate(
        revisit_days = difftime(
            admit.datetime,
            discharge.datetime,
            units = "days"
        )
    ) %>%
    filter(
        revisit_days > 0,
        revisit_days <= 30
    )

gynonc_revisit_mbo <- concat_encounters(gynonc_encounters$millennium.id)

# run MBO query:
#   * Visit Data

# revisit admit type and source
data_revisit <- read_data(dir_raw, "revisit", FALSE) %>%
    as.visits() %>%
    inner_join(
        gynonc_encounters[c(
            "millennium.id",
            "pie.id",
            "person.id",
            "revisit_days"
        )],
        by = "millennium.id"
    ) %>%
    rename(
        revisit_mid = millennium.id,
        revisit_pie = pie.id
    ) %>%
    left_join(
        gynonc_id[c("millennium.id", "pie.id", "person.id")],
        by = "person.id"
    ) %>%
    select(-(arrival.datetime:discharge.datetime))

# pain meds --------------------------------------------

opiods <- tibble(
    name = c(
        "narcotic analgesics",
        "narcotic analgesic combinations",
        "acetaminophen"
    ),
    type = c(rep("class", 2), "med"),
    group = "sched"
)

cont_opiods <- tibble(
    name = c(
        "fentanyl",
        "morphine",
        "hydromorphone",
        "mepiridine",
        "remifentanyl",
        "sufentanyl"
    ),
    type = "med",
    group = "cont"
)

lookup_meds <- med_lookup(
    c(
        "narcotic analgesics",
        "narcotic analgesic combinations"
    )
) %>%
    mutate_at("med.name", str_to_lower)

meds_pain <- meds %>%
    filter(med %in% c(lookup_meds$med.name, "acetaminophen")) %>%
    inner_join(
        data_patients[c("millennium.id", "room_out", "arrive.datetime")],
        by = "millennium.id"
    ) %>%
    mutate(
        timing = case_when(
            med.datetime < room_out ~ "or",
            med.datetime < arrive.datetime ~ "pacu",
            TRUE ~ "floor"
        ),
        time_surg = difftime(
            med.datetime,
            room_out,
            units = "hours"
        )
    )

mbo_order_id <- concat_encounters(meds_pain$orig.order.id)

# run MBO query:
#   * Orders Meds - Details - by Order Id

meds_orders <- read_data(dir_raw, "orders", FALSE) %>%
    as.order_detail(extras = list("med.product" = "Mnemonic (Product)")) %>%
    rename(
        frequency = freq,
        order.route = route
    )

data_pain_meds <- meds_pain %>%
    filter(time_surg < 24) %>%
    left_join(
        meds_orders,
        by = c(
            "millennium.id",
            "orig.order.id" = "order.id"
        )
    ) %>%
    add_count(
        millennium.id,
        med,
        med.dose.units,
        route,
        timing
    ) %>%
    rename(num_doses = n) %>%
    group_by(
        millennium.id,
        med,
        med.product,
        med.dose.units,
        route,
        frequency,
        timing,
        num_doses
    ) %>%
    summarize_at("med.dose", sum, na.rm = TRUE) %>%
    arrange(millennium.id, med)

opiods <- med_lookup(
    c(
        "narcotic analgesics",
        "narcotic analgesic combinations"
    )
) %>%
    mutate_at("med.name", str_to_lower)

data_opiods <- data_pain_meds %>%
    filter(med %in% opiods$med.name) %>%
    calc_morph_eq()

data_meds_cont <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    calc_runtime() %>%
    summarize_data() %>%
    filter(
        med %in% lookup_meds$med.name,
        cum.dose > 0
    )

# pain scores ------------------------------------------

pain_scores <- read_data(dir_raw, "pain-scores", FALSE) %>%
    as.pain_scores() %>%
    semi_join(gynonc_id, by = "millennium.id") %>%
    left_join(data_patients, by = "millennium.id") %>%
    filter(event.datetime > surgery_start) %>%
    mutate(
        time_surg = difftime(
            event.datetime,
            surgery_stop,
            units = "hours"
        )
    )

pain_avg_all <- pain_scores %>%
    filter(time_surg <= 24) %>%
    rename(
        vital.datetime = event.datetime,
        vital = event,
        vital.result = event.result
    ) %>%
    mutate_at("vital.result", as.numeric)

pain_avg_prior <- pain_scores %>%
    filter(
        time_surg <= 24,
        order.id == "0"
    ) %>%
    rename(
        vital.datetime = event.datetime,
        vital = event,
        vital.result = event.result
    ) %>%
    mutate_at("vital.result", as.numeric)

class(pain_avg_all) <- append(
    class(pain_avg_all),
    c("vitals", "tbl_edwr"),
    after = 0L
)

attr(pain_avg_all, "data") <- "mbo"

data_pain_scores_all <- pain_avg_all %>%
    calc_runtime() %>%
    summarize_data()

class(pain_avg_prior) <- append(
    class(pain_avg_prior),
    c("vitals", "tbl_edwr"),
    after = 0L
)

attr(pain_avg_prior, "data") <- "mbo"

data_pain_scores_prior <- pain_avg_prior %>%
    calc_runtime() %>%
    summarize_data()

# pca --------------------------------------------------

pca_actions <- c(
    # "pca continuous rate dose" = "pca_rate",
    "pca demand dose unit" = "pca_dose_unit",
    "pca demand dose" = "pca_dose",
    "pca doses delivered" = "pca_delivered",
    "pca drug" = "pca_drug",
    "pca loading dose" = "pca_load",
    "pca lockout interval \\(minutes\\)" = "pca_lockout",
    "pca total demands" = "pca_demands"
)

pain_pca <- read_data(dir_raw, "pain-pca", FALSE) %>%
    as.pain_scores() %>%
    semi_join(gynonc_id, by = "millennium.id") %>%
    select(millennium.id:event.result) %>%
    mutate_at("event", str_replace_all, pattern = pca_actions) %>%
    distinct() %>%
    spread(event, event.result) %>%
    mutate_at(
        c(
            "pca_demands",
            "pca_dose",
            "pca_delivered",
            "pca_load",
            "pca_lockout"
            # "pca_rate"
        ),
        as.numeric
    ) %>%
    left_join(
        data_patients[c(
            "millennium.id",
            "room_out",
            "depart.datetime"
        )],
        by = "millennium.id"
    ) %>%
    group_by(millennium.id, event.datetime) %>%
    mutate(
        total_dose = sum(
            pca_delivered * pca_dose,
            pca_load,
            na.rm = TRUE
        ),
        postop_day = difftime(
            floor_date(event.datetime, "day"),
            floor_date(room_out, "day"),
            units = "days"
        ),
        time_surg = difftime(
            event.datetime,
            room_out,
            units = "hours"
        )
    )

data_pca <- pain_pca %>%
    # group_by(pie.id, postop_day, pca_drug) %>%
    # filter(time_surg < 24) %>%
    filter(postop_day <= 1) %>%
    group_by(millennium.id, pca_drug) %>%
    summarize_at(
        c(
            "pca_demands",
            "pca_delivered",
            "total_dose"
        ),
        sum,
        na.rm = TRUE
    )

# home pain meds ---------------------------------------

meds_home <- read_data(dir_raw, "meds-home", FALSE) %>%
    as.meds_home() %>%
    filter(med %in% lookup_meds$med.name) %>%
    distinct(millennium.id) %>%
    mutate(home_pain_med = TRUE)

# demographics -----------------------------------------

data_demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics() %>%
    semi_join(data_patients, by = "millennium.id") %>%
    left_join(meds_home, by = "millennium.id") %>%
    mutate_at("home_pain_med", funs(coalesce(., FALSE)))

# nausea meds ------------------------------------------

nv_meds <- med_lookup(
    c(
        "5HT3 receptor antagonists",
        "phenothiazine antiemetics"
    )
) %>%
    mutate_at("med.name", str_to_lower)

meds_nausea <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med %in% nv_meds$med.name) %>%
    inner_join(
        data_patients[c(
            "millennium.id",
            "room_out",
            "arrive.datetime"
        )],
        by = "millennium.id"
    ) %>%
    mutate(
        timing = case_when(
            med.datetime < room_out ~ "or",
            med.datetime < arrive.datetime ~ "pacu",
            TRUE ~ "floor"
        ),
        time_surg = difftime(
            med.datetime,
            room_out,
            units = "hours"
        )
    )

data_nv_meds <- meds_nausea %>%
    filter(time_surg < 24) %>%
    add_count(
        millennium.id,
        med,
        med.dose.units,
        route,
        timing
    ) %>%
    rename(num_doses = n) %>%
    group_by(
        millennium.id,
        med,
        med.dose.units,
        route,
        timing,
        num_doses
    ) %>%
    summarize_at("med.dose", sum, na.rm = TRUE) %>%
    arrange(millennium.id, med)

# emesis
# number prn

dir_save <- paste0("data/tidy/report/", begin_abbrev, "_", end_abbrev)

if (!dir.exists(dir_save)) dir.create(dir_save)

dirr::save_rds(dir_save, "^data_")
