library(tidyverse)
library(edwr)
library(lubridate)
library(stringr)
library(icd)

dir_raw <- "data/raw/pilot1"

# run MBO query
#   * Patients - by Discharge Unit
#       - Facility (Curr): HH HERMANN
#       - Nurse Unit (Curr): HH 6EJP;HH 6WJP
#       - Date Only - Admit: 1/29/2018 - 2/28/2018

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(
        discharge.datetime >= mdy("1/29/2018", tz = "US/Central"),
        discharge.datetime < mdy("2/28/2018", tz = "US/Central"),
        visit.type == "Inpatient"
    )

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO queries
#   * Demographics
#   * Diagnosis - ICD-9/10-CM
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
    sample_n(110)

id <- read_data(dir_raw, "identifiers") %>%
    as.id()

icd <- read_data(dir_raw, "diagnosis", FALSE) %>%
    as.diagnosis()

primary <- icd %>%
    filter(
        diag.type == "FINAL",
        diag.seq == "Primary"
    ) %>%
    mutate_at("diag.code", as.icd10) %>%
    mutate(
        "icd" = icd_decimal_to_short(diag.code),
        desc = icd_explain_table(icd)$short_desc
    )

# locations <- read_data(dir_raw, "location", FALSE) %>%
#     as.locations() %>%
#     filter(unit.name %in% units) %>%
#     semi_join(demog, by = "millennium.id")

scores <- read_data(dir_raw, "^pain-scores", FALSE) %>%
    as.pain_scores() %>%
    filter(event.location %in% units)

pain_meds <- med_lookup("analgesics") %>%
    mutate_at("med.name", str_to_lower)

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(
        med %in% pain_meds$med.name,
        med != "aspirin",
        med.location %in% units
    )

orders <- meds %>%
    mutate(order_id = order.parent.id) %>%
    mutate_at("order_id", funs(na_if(., 0))) %>%
    mutate_at("order_id", funs(coalesce(., order.id)))

mbo_order <- concat_encounters(orders$order_id)

# run MBO query
#   * Orders Meds - Details - by Order Id

details <- read_data(dir_raw, "orders", FALSE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        order_id = `Order Id`,
        freq = Frequency,
        prn = `PRN Indicator`
    )

set.seed(77123)
data_patients <- demog %>%
    left_join(primary, by = "millennium.id") %>%
    left_join(id, by = "millennium.id") %>%
    filter(
        !is.na(diag.code),
        !is.na(fin)
    ) %>%
    sample_n(100) %>%
    select(millennium.id, fin, age, gender, diag.code, desc)

data_meds <- orders %>%
    left_join(details, by = c("millennium.id", "order_id")) %>%
    semi_join(data_patients, by = "millennium.id") %>%
    select(millennium.id, med.datetime:route, freq, prn, event.tag)

data_scores <- scores %>%
    semi_join(data_patients, by = "millennium.id") %>%
    select(millennium.id:event.result)

write_csv(data_patients, "data/external/pilot/patients.csv")
write_csv(data_meds, "data/external/pilot/meds.csv")
write_csv(data_scores, "data/external/pilot/scores.csv")
