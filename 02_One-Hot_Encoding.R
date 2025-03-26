
library(fastDummies)

LS22_23_encoded <- fastDummies::dummy_cols(no_na_data, select_columns = c("overall_classification", "overall_primary_purpose"), remove_first_dummy = TRUE)
LS22_23_encoded <- LS22_23_encoded %>%
    clean_names()