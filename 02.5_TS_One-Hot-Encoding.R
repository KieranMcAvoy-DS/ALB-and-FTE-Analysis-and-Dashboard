library(fastDummies)

TS_no_na_data$classification[TS_no_na_data$classification == "Crown NDPB"] <- "NDPB"
TS_no_na_data$parent_department[TS_no_na_data$parent_department == "Her Majesty's Treasury"] <- "HM Treasury"
TS_no_na_data$parent_department[TS_no_na_data$parent_department == "department for culture, media and sport"] <- "department for digital, culture, media and Ssport"
TS_encoded <- fastDummies::dummy_cols(TS_no_na_data, select_columns = c("classification", "parent_department"), remove_first_dummy = TRUE)
TS_encoded <- TS_encoded %>%
    clean_names()