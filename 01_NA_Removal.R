
# setwd("/Users/kieran.mcavoy/Documents/FTE and Budget Project/FTE-Budget-Project")

LS22_23$staffing_fte_inpost[LS22_23$staffing_fte_inpost == 0] <- NA
LS22_23$finance_budget_overall[LS22_23$finance_budget_overall < 10] <- NA
LS22_23$finance_spend_rdel_overall[LS22_23$finance_spend_rdel_overall == 0] <- NA
LS22_23$finance_spend_rdel_overall[LS22_23$finance_spend_rdel_overall < 0] <- NA
LS22_23$staffing_pct_fte_london <- as.numeric(LS22_23$staffing_pct_fte_london)
LS22_23$finance_budget_other_income <- as.character(LS22_23$finance_budget_other_income)

no_na_data <- LS22_23 |> 
    filter(!is.na(staffing_fte_inpost)) |> # remove rows of NAs in fte 
    filter(!is.na(finance_budget_govt_income)) |># remove rows of NAs in govt income
    filter(!is.na(finance_spend_rdel_overall)) |>
    filter(!is.na(finance_budget_overall)) 