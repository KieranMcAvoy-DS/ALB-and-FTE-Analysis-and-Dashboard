LS22_23_vis <- no_na_data
LS22_23_vis$staffing_fte_inpost[LS22_23_vis$staffing_fte_inpost == 0] <- NA
LS22_23_vis$staffing_fte_inpost[LS22_23_vis$staffing_fte_inpost > 2000] <- NA
LS22_23_vis$finance_budget_overall[LS22_23_vis$finance_budget_overall < 10] <- NA
LS22_23_vis$finance_spend_rdel_overall[LS22_23_vis$finance_spend_rdel_overall == 0] <- NA
LS22_23_vis$staffing_fte_employed_in_london[LS22_23_vis$staffing_fte_employed_in_london == 0] <- NA
LS22_23_vis$staffing_fte_employed_in_london[LS22_23_vis$staffing_fte_employed_in_london > 1000] <- NA
LS22_23_vis$finance_budget_overall[LS22_23_vis$finance_budget_overall > 150000000] <- NA

LS22_23_vis<- LS22_23_vis |> 
    filter(!is.na(staffing_fte_inpost)) |> # remove rows of NAs in fte 
    filter(!is.na(finance_budget_govt_income)) |># remove rows of NAs in govt income
    filter(!is.na(staffing_fte_employed_in_london)) |>
    filter(!is.na(finance_spend_rdel_overall)) |>
    filter(!is.na(finance_budget_overall))

ggplot(LS22_23_vis, aes(x=staffing_fte_inpost, y=finance_budget_overall, color=overall_classification)) + 
           ggtitle("Figure 1: Scatter plot of ALB Budget and FTE for Financial Year 2022/23")+
                           xlab("Total Staff (FTE)")+
                           ylab("Total ALB Budget (£)") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(colour = "Classification") +
    geom_point()
