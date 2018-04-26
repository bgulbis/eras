library(tidyverse)
library(lubridate)
library(broom)
library(officer)
library(themebg)
library(ggrepel)
library(rvg)

tz <- "US/Central"
brew_pal <- "Set1"

begin_month <- mdy("7/1/2017", tz = tz)
begin_abbrev <- format(begin_month, "%Y-%m")

end_month <- mdy("3/1/2018", tz = tz)
end_abbrev <- format(end_month, "%Y-%m")

dir_tidy <- paste0("data/tidy/report/", begin_abbrev, "_", end_abbrev)

x <- dirr::get_rds(dir_tidy)

data_patients <- data_patients %>%
    mutate(surg_month = floor_date(surgery_start, "month")) %>%
    mutate_if(is.difftime, as.numeric)

fig_num_surgery <- data_patients %>%
    ungroup() %>%
    mutate_at(
        "surgery",
        funs(coalesce(., "Unknown"))
    ) %>%
    mutate_at(
        "surgery",
        fct_lump,
        n = 3
    ) %>%
    count(surg_month, surgery) %>%
    ggplot(aes(x = surg_month, y = n)) +
    geom_bar(aes(fill = surgery), stat = "identity") +
    scale_fill_brewer(
        NULL,
        palette = brew_pal
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    ylab("Patients (N)") +
    theme_bg() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2))

fig_surgery_by_surgeon <- data_patients %>%
    ungroup() %>%
    mutate_at(
        c("surgery", "surgeon"),
        funs(coalesce(., "Unknown"))
    ) %>%
    mutate_at(
        c("surgery", "surgeon"),
        fct_lump,
        n = 3
    ) %>%
    count(surg_month, surgeon, surgery) %>%
    ggplot(aes(x = surg_month, y = n)) +
    geom_bar(aes(fill = surgery), stat = "identity") +
    scale_fill_brewer(
        NULL,
        palette = brew_pal
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    ylab("Patients (N)") +
    facet_wrap(~ surgeon, ncol = 1) +
    theme_bg() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2))

fig_los <- data_patients %>%
    select(
        millennium.id,
        surg_month,
        or.hours = or_hours,
        pacu.hours = pacu_hours,
        postop.los = postop_los
    ) %>%
    left_join(
        data_demographics[c("millennium.id", "length.stay")],
        by = "millennium.id"
    ) %>%
    ungroup() %>%
    gather(key, value, or.hours:length.stay) %>%
    mutate_at(
        "key",
        factor,
        levels = c(
            "length.stay",
            "postop.los",
            "pacu.hours",
            "or.hours"
        ),
        labels = c(
            "Hospital LOS (days)",
            "Post-op LOS (days)",
            "PACU Duration (hours)",
            "OR Duration (hours)"
        )
    ) %>%
    ggplot(aes(x = surg_month, y = value)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess") +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    ylab("Time") +
    facet_wrap(~key, ncol = 1) +
    coord_cartesian(ylim = c(0, 8)) +
    theme_bg()

df <- data_patients %>%
    select(
        millennium.id,
        surg_month,
        surgeon,
        admit.datetime
    ) %>%
    arrange(surgeon, admit.datetime) %>%
    left_join(
        data_demographics[c("millennium.id", "length.stay")],
        by = "millennium.id"
    ) %>%
    ungroup() %>%
    mutate_at(
        "surgeon",
        fct_lump,
        n = 3
    ) %>%
    group_by(surgeon)

fit <- df %>%
    mutate_at("admit.datetime", as.numeric) %>%
    do(augment(loess(length.stay ~ admit.datetime, .))) %>%
    filter(admit.datetime == max(admit.datetime)) %>%
    select(-admit.datetime) %>%
    left_join(
        df %>%
            select(surgeon, admit.datetime) %>%
            filter(admit.datetime == max(admit.datetime)),
        by = "surgeon"
    )

fig_los_surgeon <- df %>%
    ggplot(aes(x = admit.datetime, y = length.stay)) +
    geom_smooth(
        aes(color = surgeon),
        method = "loess",
        se = FALSE
    ) +
    geom_text_repel(
        data = fit,
        aes(
            label = surgeon,
            x = admit.datetime,
            y = .fitted,
            color = surgeon
        ),
        size = 3,
        na.rm = TRUE,
        min.segment.length = 5
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_color_brewer(NULL, palette = brew_pal, guide = "none") +
    scale_y_continuous("Length of stay (days)", breaks = 0:8) +
    coord_cartesian(ylim = c(0, 8)) +
    theme_bg()

df <- data_patients %>%
    select(
        millennium.id,
        surg_month,
        surgery,
        admit.datetime
    ) %>%
    arrange(surgery, admit.datetime) %>%
    left_join(
        data_demographics[c("millennium.id", "length.stay")],
        by = "millennium.id"
    ) %>%
    ungroup() %>%
    add_count(surgery) %>%
    mutate_at(
        "surgery",
        funs(coalesce(., "Other"))
    ) %>%
    mutate_at(
        "surgery",
        fct_lump,
        n = 3
    ) %>%
    # filter(
    #     n > 1,
    #     !is.na(surgery)
    # ) %>%
    group_by(surgery)

fit <- df %>%
    mutate_at("admit.datetime", as.numeric) %>%
    do(augment(loess(length.stay ~ admit.datetime, .))) %>%
    filter(admit.datetime == max(admit.datetime)) %>%
    select(-admit.datetime) %>%
    left_join(
        df %>%
            select(surgery, admit.datetime) %>%
            filter(admit.datetime == max(admit.datetime)),
        by = "surgery"
    )

fig_los_surgery <- df %>%
    ggplot(aes(x = admit.datetime, y = length.stay)) +
    geom_smooth(
        aes(color = surgery),
        method = "loess",
        se = FALSE
    ) +
    geom_text_repel(
        data = fit,
        aes(
            label = surgery,
            x = admit.datetime,
            y = .fitted,
            color = surgery
        ),
        size = 3,
        nudge_y = -0.1,
        na.rm = TRUE,
        min.segment.length = 5
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_color_brewer(NULL, palette = brew_pal, guide = "none") +
    scale_y_continuous("Length of stay (days)", breaks = 0:8) +
    coord_cartesian(ylim = c(0, 8)) +
    theme_bg()

num <- data_patients %>%
    ungroup() %>%
    count(surg_month) %>%
    rename(pts = n)

fig_dc_pod1 <- data_patients %>%
    ungroup() %>%
    mutate_at("dc_day", funs(. == 1)) %>%
    group_by(surg_month) %>%
    summarize_at("dc_day", sum, na.rm = TRUE) %>%
    left_join(num, by = "surg_month") %>%
    mutate(pct.pod = dc_day / pts * 100) %>%
    ggplot(aes(x = surg_month, y = dc_day)) +
    geom_bar(stat = "identity") +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_fill_brewer(NULL, palette = brew_pal) +
    xlab(NULL) +
    ylab("Patients (%)") +
    theme_bg() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 1)) +
    coord_cartesian(ylim = c(0, 100))

fig_dc_pod1_surgery <- data_patients %>%
    group_by(surg_month) %>%
    add_count(surgery) %>%
    group_by(surg_month, surgery, n) %>%
    summarize_at("dc_day", funs(sum(. == 1, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(pct.pod = dc_day / n * 100) %>%
    mutate_at(
        "surgery",
        funs(coalesce(., "Other"))
    ) %>%
    mutate_at(
        "surgery",
        fct_lump,
        n = 3
    ) %>%
    # filter(
    #     n > 1,
    #     !is.na(surgery)
    # ) %>%
    ggplot(aes(x = surg_month, y = pct.pod)) +
    geom_bar(
        # aes(fill = surgery),
        stat = "identity",
        position = "dodge"
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_fill_brewer(NULL, palette = brew_pal) +
    facet_wrap(~ surgery, ncol = 1) +
    xlab(NULL) +
    ylab("Patients (%)") +
    theme_bg()

fig_readmit <- data_revisit %>%
    left_join(data_patients, by = "millennium.id") %>%
    filter(admit.type != "Preadmit Not OB") %>%
    count(surg_month, visit.type) %>%
    spread(visit.type, n) %>%
    gather(visit.type, n, Inpatient, Observation) %>%
    mutate_at("n", funs(coalesce(., 0L))) %>%
    ungroup() %>%
    full_join(num, by = "surg_month") %>%
    mutate(pct_pt = n / pts * 100) %>%
    filter(!is.na(visit.type)) %>%
    ggplot(aes(x = surg_month, y = pct_pt)) +
    geom_bar(
        aes(fill = visit.type),
        stat = "identity",
        position = "stack"
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_fill_brewer(NULL, palette = brew_pal) +
    xlab(NULL) +
    ylab("Patients (%)") +
    theme_bg() +
    theme(legend.position = "bottom") +
    coord_cartesian(ylim = c(0, 100))

df <- data_pain_scores_all %>%
    select(
        millennium.id,
        first.result:max.result,
        time.wt.avg
    ) %>%
    gather(
        key,
        value,
        first.result,
        last.result,
        max.result,
        time.wt.avg
    ) %>%
    mutate_at(
        "key",
        factor,
        levels = c(
            "first.result",
            "last.result",
            "max.result",
            "time.wt.avg"
        ),
        labels = c(
            "First",
            "Last",
            "Max",
            "Average"
        )
    ) %>%
    group_by(key)

fit <- df %>%
    mutate_at("last.datetime", as.numeric) %>%
    do(augment(loess(value ~ last.datetime, .))) %>%
    filter(last.datetime == max(last.datetime)) %>%
    select(-last.datetime) %>%
    left_join(
        df %>%
            select(key, last.datetime) %>%
            filter(last.datetime == max(last.datetime)),
        by = "key"
    )

fig_pain <- df %>%
    ggplot(aes(x = last.datetime, y = value)) +
    geom_smooth(
        aes(color = key),
        method = "loess",
        se = FALSE
    ) +
    geom_text_repel(
        data = fit,
        aes(
            label = key,
            x = last.datetime,
            y = .fitted,
            color = key
        ),
        size = 3,
        na.rm = TRUE,
        min.segment.length = 5
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_color_brewer(NULL, palette = brew_pal, guide = "none") +
    scale_y_continuous("Pain score", breaks = seq(0, 10, by = 2)) +
    coord_cartesian(ylim = c(0, 10)) +
    theme_bg()

df <- data_pain_scores_prior %>%
    select(
        millennium.id,
        first.result:max.result,
        time.wt.avg
    ) %>%
    gather(
        key,
        value,
        first.result,
        last.result,
        max.result,
        time.wt.avg
    ) %>%
    mutate_at(
        "key",
        factor,
        levels = c(
            "first.result",
            "last.result",
            "max.result",
            "time.wt.avg"
        ),
        labels = c(
            "First",
            "Last",
            "Max",
            "Average"
        )
    ) %>%
    filter(value >= 0) %>%
    group_by(key)

fit <- df %>%
    mutate_at("last.datetime", as.numeric) %>%
    do(augment(loess(value ~ last.datetime, .))) %>%
    filter(last.datetime == max(last.datetime)) %>%
    select(-last.datetime) %>%
    left_join(
        df %>%
            select(key, last.datetime) %>%
            filter(last.datetime == max(last.datetime)),
        by = "key"
    )

fig_pain_premed <- df %>%
    ggplot(aes(x = last.datetime, y = value)) +
    geom_smooth(
        aes(color = key),
        method = "loess",
        se = FALSE
    ) +
    geom_text_repel(
        data = fit,
        aes(
            label = key,
            x = last.datetime,
            y = .fitted,
            color = key
        ),
        size = 3,
        na.rm = TRUE,
        min.segment.length = 5
    ) +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    scale_color_brewer(NULL, palette = brew_pal, guide = "none") +
    scale_y_continuous("Pain score", breaks = seq(0, 10, by = 2)) +
    coord_cartesian(ylim = c(0, 10)) +
    theme_bg()

df <- data_pain_meds %>%
    ungroup() %>%
    inner_join(
        data_patients[c("millennium.id", "surg_month")],
        by = "millennium.id"
    ) %>%
    mutate_at(
        "route",
        str_replace_all,
        pattern = c(
            "IVPB" = "IV",
            "IVP" = "IV",
            "NG" = "PO"
        )
    )

total_doses <- df %>%
    group_by(surg_month) %>%
    summarize_at(
        "num_doses",
        funs(total_doses = sum(., na.rm = TRUE))
    )

fig_med_location <- df %>%
    group_by(surg_month, timing, route) %>%
    summarize_at("num_doses", sum, na.rm = TRUE) %>%
    left_join(total_doses, by = "surg_month") %>%
    mutate(pct_doses = num_doses / total_doses * 100) %>%
    ungroup() %>%
    mutate_at(
        "timing",
        str_replace_all,
        pattern = c(
            "floor" = "Floor",
            "^or" = "OR",
            "pacu" = "PACU"
        )
    ) %>%
    ggplot(aes(x = surg_month, y = pct_doses, fill = route)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    ylab("Doses (%)") +
    scale_fill_brewer(NULL, palette = brew_pal) +
    facet_wrap(~ timing, ncol = 1) +
    theme_bg() +
    theme(legend.position = "bottom") +
    coord_cartesian(ylim = c(0, 100))

df_opiods <- data_opiods %>%
    mutate(total.mme = mme.iv * num_doses) %>%
    left_join(data_patients, by = "millennium.id") %>%
    group_by(millennium.id, surg_month, timing) %>%
    summarize_at("total.mme", sum, na.rm = TRUE) %>%
    ungroup() %>%
    arrange(surg_month) %>%
    mutate_at(
        "surg_month",
        format,
        format = "%b"
    ) %>%
    mutate_at(
        "surg_month",
        as_factor
    ) %>%
    mutate_at(
        "timing",
        str_replace_all,
        pattern = c(
            "floor" = "Floor",
            "^or" = "OR",
            "pacu" = "PACU"
        )
    )

fig_opiod_floor <- df_opiods %>%
    filter(timing == "Floor") %>%
    ggplot(aes(x = surg_month, y = total.mme)) +
    geom_boxplot() +
    xlab(NULL) +
    ylab("IV Morphine Equivalents") +
    coord_cartesian(ylim = c(0, 300)) +
    theme_bg()

fig_pca <- data_patients %>%
    select(millennium.id, surg_month) %>%
    left_join(data_pca, by = "millennium.id") %>%
    mutate(pca = !is.na(pca_drug)) %>%
    group_by(surg_month) %>%
    summarize_at("pca", sum) %>%
    left_join(num, by = "surg_month") %>%
    ungroup() %>%
    mutate(pct_pca = pca / pts * 100) %>%
    ggplot(aes(x = surg_month, y = pct_pca)) +
    geom_bar(stat = "identity") +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    ylab("Patients (%)") +
    scale_fill_brewer(NULL, palette = brew_pal) +
    theme_bg() +
    coord_cartesian(ylim = c(0, 100))

fig_home_opiods <- data_demographics %>%
    left_join(data_patients, by = "millennium.id") %>%
    group_by(surg_month) %>%
    summarize_at("home_pain_med", sum, na.rm = TRUE) %>%
    ungroup() %>%
    full_join(num, by = "surg_month") %>%
    mutate(pct_pt = home_pain_med / pts * 100) %>%
    ggplot(aes(x = surg_month, y = pct_pt)) +
    geom_bar(stat = "identity") +
    scale_x_datetime(
        NULL,
        date_breaks = "1 month",
        date_labels = "%b"
    ) +
    xlab(NULL) +
    ylab("Patients (%)") +
    theme_bg() +
    coord_cartesian(ylim = c(0, 100))

fig_nv <- data_nv_meds %>%
    filter(timing == "floor") %>%
    left_join(data_patients, by = "millennium.id") %>%
    group_by(millennium.id, surg_month) %>%
    summarize_at("num_doses", sum, na.rm = TRUE) %>%
    ungroup() %>%
    arrange(surg_month) %>%
    mutate_at(
        "surg_month",
        format,
        format = "%b"
    ) %>%
    mutate_at(
        "surg_month",
        as_factor
    ) %>%
    ggplot(aes(x = surg_month, y = num_doses)) +
    geom_boxplot() +
    xlab(NULL) +
    scale_y_continuous("Nausea Medications (N)", breaks = seq(0, 10, by = 2)) +
    # coord_cartesian(ylim = c(0, 300)) +
    theme_bg()

# powerpoint -------------------------------------------

slide_layout <- "Title and Content"
slide_master <- "Office Theme"
month_abbrv <- format(end_month, "%Y-%m")

text_style <- fp_text(
    font.size = 32,
    font.family = "Calibri"
)

read_pptx() %>%
    add_slide(layout = "Title Slide", master = slide_master) %>%
    ph_with_text(
        type = "ctrTitle",
        str = "ERAS in Bariatric Surgery"
    ) %>%
    ph_with_text(
        type = "subTitle",
        str = "Brian Gulbis, PharmD, BCPS"
    ) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Criteria"
    ) %>%
    ph_with_text(
        type = "body",
        str = "Patients admitted between July 1, 2017 and March 31, 2018"
    ) %>%
    ph_add_par(type = "body") %>%
    ph_add_text(
        type = "body",
        str = "ICD-10 Procedure Codes: 0DB64Z3, 0DB64ZZ, 0D164ZA",
        style = text_style
    ) %>%
    ph_add_par(type = "body") %>%
    ph_add_text(
        type = "body",
        str = "Elective surgeries",
        style = text_style
    ) %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Number of bariatric surgeries"
    ) %>%
    ph_with_vg(ggobj = fig_num_surgery, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Surgeries by surgeon"
    ) %>%
    ph_with_vg(ggobj = fig_surgery_by_surgeon, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Length of stay trends"
    ) %>%
    ph_with_vg(ggobj = fig_los, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Length of stay trend by surgeon"
    ) %>%
    ph_with_vg(ggobj = fig_los_surgeon, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Length of stay trend by surgery"
    ) %>%
    ph_with_vg(ggobj = fig_los_surgery, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Discharged on POD#1"
    ) %>%
    ph_with_vg(ggobj = fig_dc_pod1, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Discharged on POD#1 by surgery"
    ) %>%
    ph_with_vg(ggobj = fig_dc_pod1_surgery, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Readmitted within 30 days"
    ) %>%
    ph_with_vg(ggobj = fig_readmit, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Pain scores during first 24 hours"
    ) %>%
    ph_with_vg(ggobj = fig_pain, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Pre-medication pain scores during first 24 hours"
    ) %>%
    ph_with_vg(ggobj = fig_pain_premed, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Pain meds administered by area"
    ) %>%
    ph_with_vg(ggobj = fig_med_location, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "IV morphine equivalents given on the floor during first 24 hours"
    ) %>%
    ph_with_vg(ggobj = fig_opiod_floor, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "PCA use"
    ) %>%
    ph_with_vg(ggobj = fig_pca, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Patients on opiods at home"
    ) %>%
    ph_with_vg(ggobj = fig_home_opiods, type = "body") %>%
    add_slide(layout = slide_layout, master = slide_master) %>%
    ph_with_text(
        type = "title",
        str = "Nausea medications"
    ) %>%
    ph_with_vg(ggobj = fig_nv, type = "body") %>%
    print(
        target = paste0(
            "report\\",
            format(end_month, "%Y-%m"),
            "_eras_bariatrics.pptx"
        )
    )
