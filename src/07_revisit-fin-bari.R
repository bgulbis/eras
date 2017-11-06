library(tidyverse)
library(edwr)

data_revisit <- read_rds("data/tidy/bari_2015/data_revisit.Rds") %>%
    bind_rows(read_rds("data/tidy/bari_baseline/data_revisit.Rds"),
              read_rds("data/tidy/bari_current/data_revisit.Rds"))

edw_pie <- concat_encounters(data_revisit$revisit_pie)

# run EDW query
#   * Identifiers - by PowerInsight Encounter Id

id <- read_data("data/raw", "id-revisit") %>%
    as.id()

data_revisit %>%
    left_join(id, by = c("revisit_pie" = "pie.id")) %>%
    select(fin, visit.type:nurse.unit.admit) %>%
    write_csv("data/external/revisits.csv")
