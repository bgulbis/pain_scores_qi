library(tidyverse)
library(edwr)
library(lubridate)
library(stringr)

dir_raw <- "data/raw"

# run MBO query
#   * Patients - by Discharge Unit

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(discharge.datetime < mdy("10/1/2017", tz = "US/Central"),
           visit.type == "Inpatient")

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO queries
#   * Demographics
#   * Location History
#   * Medications - Inpatient - All
#   * Pain Scores

# run EDW query
#   * Identifiers - by Millennium Encounter ID

units <- c("HH 6EJP", "HH 6WJP")

set.seed(77123)
demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics() %>%
    filter(age >= 18) %>%
    sample_n(100)

locations <- read_data(dir_raw, "location", FALSE) %>%
    as.locations() %>%
    filter(unit.name %in% units) %>%
    semi_join(demog, by = "millennium.id")

scores <- read_data(dir_raw, "^pain-scores", FALSE) %>%
    as.pain_scores() %>%
    semi_join(demog, by = "millennium.id") %>%
    filter(event.location %in% units)

pain_meds <- med_lookup("analgesics") %>%
    mutate_at("med.name", str_to_lower)

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med %in% pain_meds$med.name,
           med != "aspirin",
           med.location %in% units)

orders <- meds %>%
    mutate(order_id = order.parent.id) %>%
    mutate_at("order_id", funs(na_if(., 0))) %>%
    mutate_at("order_id", funs(coalesce(., order.id)))

mbo_order <- concat_encounters(orders$order_id)

# run MBO query
#   * Orders Meds - Details - by Order Id

details <- read_data(dir_raw, "orders", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           order_id = `Order Id`,
           freq = Frequency,
           prn = `PRN Indicator`)

data_meds <- orders %>%
    left_join(details, by = c("millennium.id", "order_id"))
