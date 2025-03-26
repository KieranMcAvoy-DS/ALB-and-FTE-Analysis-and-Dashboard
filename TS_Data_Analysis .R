#Analysis of the ALB budget and FTE TS data
source("./00_Import_Nec_Libs.R")
library(scales)
TS_Data <- readxl::read_xlsx("KM Data Slice/2024-06-21 -LIVE- Consolidated PBD and CS Workforce Data.xlsx")
options(scipen = 999)
# Exploratory Analysis ----
hist(TS_Data$finance_budget_govt_funding)
hist(log(TS_Data$finance_budget_govt_funding))

TS_no_na_data <- TS_Data |> 
    filter(!is.na(fte)) # remove rows of NAs in fte

# Removing outliers where necessary and completing transformations ----
TS_no_na_data$fte <- str_replace_all(TS_no_na_data$fte, "(?<=\\d),(?=\\d)", "")
TS_no_na_data$fte <- as.numeric(TS_no_na_data$fte)


TS_no_na_data$fte[TS_no_na_data$fte == 0] <- NA
TS_no_na_data$fte[TS_no_na_data$finance_budget_govt_funding == 0] <- NA
TS_no_na_data$time[TS_no_na_data$time == 2013] <- NA
TS_no_na_data$time[TS_no_na_data$time == 2012] <- NA
TS_no_na_data$time[TS_no_na_data$time == 2022] <- NA
TS_no_na_data <- TS_no_na_data |> 
    filter(!is.na(fte)) |>
    filter(!is.na(finance_budget_govt_funding)) |>
    filter(!is.na(time))

my_data_year_sum_fte <- TS_no_na_data %>%
    group_by(time) %>% # or use year here if variable is called year
    summarise(sum_fte = sum(fte, na.rm = TRUE))

my_data_year_sum_budget <- TS_no_na_data %>%
    group_by(time) %>% 
    summarise(sum_budget = sum(finance_budget_govt_funding, na.rm = TRUE))

# Running models ----
source("02.5_TS_One-Hot-Encoding.R")
TS_model1 <- lm(log(fte) ~ time + classification_ndpb + classification_non_ministerial_department + parent_department_cabinet_office + parent_department_department_for_business_and_trade + parent_department_department_for_business_energy_and_industrial_strategy + parent_department_department_for_business_innovation_and_skills + parent_department_department_for_communities_and_local_government + parent_department_department_for_digital_culture_media_and_sport + parent_department_department_for_education + parent_department_department_for_energy_security_and_net_zero + parent_department_department_for_environment_food_and_rural_affairs + parent_department_department_for_international_development + parent_department_department_for_levelling_up_housing_and_communities + parent_department_department_for_science_innovation_and_technology + parent_department_department_for_transport + parent_department_department_for_work_and_pensions + parent_department_department_of_energy_and_climate_change + parent_department_department_of_health + parent_department_department_of_health_and_social_care + parent_department_food_standards_agency + parent_department_foreign_and_commonwealth_office + parent_department_foreign_commonwealth_and_development_office + parent_department_forestry_commission + parent_department_her_majestys_revenue_and_customs + parent_department_hm_revenue_and_customs + parent_department_hm_treasury + parent_department_home_office + parent_department_ministry_of_defence + parent_department_ministry_of_housing_communities_and_local_government + parent_department_ministry_of_justice + parent_department_northern_ireland_office + parent_department_office_of_the_secretary_of_state_for_scotland + parent_department_scotland_office + parent_department_supreme_court_of_the_united_kingdom + parent_department_wales_office, data = TS_encoded)
summary(TS_model1)
tab_model(TS_model1)
plot(x = jitter(predict(TS_model1)), y = jitter(log(TS_encoded$fte)),
     xlab="Predicted Values",
     ylab = "Actual Values",
     main = "Predicted vs Actual Values")
abline (a=0, b=1)
qqPlot(TS_model1, main = "QQ Plot of Time Series regression model (FTE as the Dependent variable)")
TS_model2 <- lm(log(finance_budget_govt_funding) ~ time + classification_ndpb + classification_non_ministerial_department + parent_department_cabinet_office + parent_department_department_for_business_and_trade + parent_department_department_for_business_energy_and_industrial_strategy + parent_department_department_for_business_innovation_and_skills + parent_department_department_for_communities_and_local_government + parent_department_department_for_digital_culture_media_and_sport + parent_department_department_for_education + parent_department_department_for_energy_security_and_net_zero + parent_department_department_for_environment_food_and_rural_affairs + parent_department_department_for_international_development + parent_department_department_for_levelling_up_housing_and_communities + parent_department_department_for_science_innovation_and_technology + parent_department_department_for_transport + parent_department_department_for_work_and_pensions + parent_department_department_of_energy_and_climate_change + parent_department_department_of_health + parent_department_department_of_health_and_social_care + parent_department_food_standards_agency + parent_department_foreign_and_commonwealth_office + parent_department_foreign_commonwealth_and_development_office + parent_department_forestry_commission + parent_department_her_majestys_revenue_and_customs + parent_department_hm_revenue_and_customs + parent_department_hm_treasury + parent_department_home_office + parent_department_ministry_of_defence + parent_department_ministry_of_housing_communities_and_local_government + parent_department_ministry_of_justice + parent_department_northern_ireland_office + parent_department_office_of_the_secretary_of_state_for_scotland + parent_department_scotland_office + parent_department_supreme_court_of_the_united_kingdom + parent_department_wales_office, data = TS_encoded)
summary(TS_model2)
install.packages("webshot")
library(webshot)
tab_model(TS_model1, TS_model2, dv.labels = c("Model 5: FTE/Time", "Model 6: Budget/Time"), file = "TS_table.html")
webshot::install_phantomjs()
webshot("TS_table.html", "TS_table.png")
qqPlot(TS_model2, main = "QQ Plot of Time Series regression model (Budget as the Dependent variable)")
vif(TS_model2)
vif(TS_model1)
exp(0.11)
exp(0.13)
# Visualisations ----
ggplot(my_data_year_sum_fte, aes(x=time, y=sum_fte)) +
    geom_line()

ggplot(my_data_year_sum_budget, aes(x=time, y=sum_budget)) +
    geom_line()

coeff <- 1000000
sums <- left_join(my_data_year_sum_fte, my_data_year_sum_budget, by = "time") %>% 
    mutate(sum_budget = sum_budget/coeff) %>% 
    pivot_longer(cols = c("sum_fte", "sum_budget"))

library(GGally)
TS.var <- TS_no_na_data[, c("time","fte","finance_budget_govt_funding")]
ggpairs(TS.var)

p1 <- ggplot(my_data_year_sum_fte, aes(x=time, y=sum_fte)) +
    geom_line(color="#69b3a2") +
    ylab("Total FTE") +
    xlab("Year") +
    ggtitle("Figure X: FTE over time") 

p2 <- ggplot(my_data_year_sum_budget, aes(x=time, y=sum_budget)) +
    geom_line(color="blue") +
    ylab("Total Budget") +
    xlab("Year") +
    ggtitle("Budget of ALB's over time") 

ggplot(sums, aes(x=time)) +
    
    geom_line( aes(y=value, group = name, color = name)) + 
    
    scale_y_continuous(labels =
                           label_number(scale_cut = cut_short_scale()),

        name = "Total FTE",
        
        sec.axis = sec_axis(~.*coeff, name="Total Government Funding (£)", labels =
                                label_number(scale_cut = cut_short_scale()))
    ) +
    ggtitle("Figure 2: Total FTE and Budget over time")

library(plotly)

plot <- plot_ly(x = my_data_year_sum_budget$time, y=(my_data_year_sum_budget$sum_budget/coeff), type = 'scatter', mode = 'lines', name = "budget", fill = 'tozeroy') %>% 
    add_trace(x = my_data_year_sum_fte$time, y = my_data_year_sum_fte$sum_fte, name = "FTE", fill = "tozeroy") %>% 
    layout(title = "Area Plot of ALB Budget Over Time",
           xaxis = list(title = "Year"),
           yaxis = list(title = "Total Budget(£)"))
plot
