#2022/23 ALB budget/FTE data analysis 
#Load Dataset----
rm(list=ls())
source("./00_Import_Nec_Libs.R") #Importing the necessary libraries
LS22_23 <- readxl::read_xlsx("KM Data Slice/2024-08-08 KM Landscape Data Slice.xlsx")
options(scipen = 999)
#NA removal and transformations ----
source("./01_NA_Removal.R") #Exclusively removes NAs from selected columns rather than entire dataset

no_na_data$log_staffing_fte_inpost <- log(no_na_data$staffing_fte_inpost)
no_na_data$log_finance_budget_overall <- log(no_na_data$finance_budget_overall)
no_na_data$log_finance_spend_rdel_overall <- log(no_na_data$finance_spend_rdel_overall)
no_na_data$Log_London_FTE <- log(no_na_data$staffing_fte_employed_in_london)

source("./02_One-Hot_Encoding.R") #One-hot encoding the variables of classification and primary purpose

#Creating Binary Variables for ALB Location 
hist(log(LS22_23$staffing_fte_employed_in_london))
LS22_23_encoded$Log_London_FTE <- log(LS22_23_encoded$staffing_fte_employed_in_london)
LS22_23_encoded$staffing_pct_fte_london <- as.numeric(LS22_23_encoded$staffing_pct_fte_london)
LS22_23_encoded$Log_London_pct <- log(LS22_23_encoded$staffing_pct_fte_london)

hist(LS22_23_encoded$staffing_pct_fte_london)

#Model Creation ----
model1 <- lm(log_staffing_fte_inpost ~ log_finance_budget_overall + log_finance_spend_rdel_overall + staffing_pct_fte_london + overall_classification_crown_ndpb + overall_classification_executive_agency + overall_classification_non_ministerial_department + overall_classification_tribunal_ndpb + overall_primary_purpose_cultural_institution + overall_primary_purpose_economic_or_sectoral_regulator + overall_primary_purpose_grant_and_subsidy_issuing_body + overall_primary_purpose_justice_prosecutorial_and_enforcement + overall_primary_purpose_major_programmes_and_delivery_organisation + overall_primary_purpose_safety_licensing_and_regulation_body, data = LS22_23_encoded)
summary(model1)
tab_model(model1)

ggplot(LS22_23_encoded, aes(x=log(staffing_fte_inpost), y=log(finance_budget_overall), main("Scatter plot of ALB Budget and FTE")+
                           xlab("Staff (FTE)")+
                           ylab("Budget(£)"))) +
    geom_point() #Linearity 
qqPlot(model1, main = "QQ Plot of 22/23 regression model") #Normality
vif(model1) #Multicollinearity (IV Independence)
plot(predict(model1), residuals(model1)) #Homoscedasticity
durbinWatsonTest(model1) #Independence of errors


source("./03_Top_Ten_Removal.R")

model2 <- lm(log_staffing_fte_inpost ~ log_finance_budget_overall + log_finance_spend_rdel_overall + staffing_pct_fte_london + overall_classification_crown_ndpb + overall_classification_executive_agency + overall_classification_non_ministerial_department + overall_classification_tribunal_ndpb + overall_primary_purpose_cultural_institution + overall_primary_purpose_economic_or_sectoral_regulator + overall_primary_purpose_grant_and_subsidy_issuing_body + overall_primary_purpose_justice_prosecutorial_and_enforcement + overall_primary_purpose_major_programmes_and_delivery_organisation + overall_primary_purpose_safety_licensing_and_regulation_body, data = no_top_ten)
summary(model2)

qqPlot(model2) #Normality
vif(model2) #Multicollinearity (IV Independence)
plot(predict(model2), residuals(model2)) #Homoscedasticity
durbinWatsonTest(model2)

tab_model(
    model1, model2,
    dv.labels = c("Model 1: Top Ten incl", "Model 2: Top Ten Removed"))

model3 <- lm(log_finance_budget_overall ~ staffing_pct_fte_london + overall_primary_purpose_cultural_institution + overall_primary_purpose_economic_or_sectoral_regulator + overall_primary_purpose_grant_and_subsidy_issuing_body + overall_primary_purpose_justice_prosecutorial_and_enforcement + overall_primary_purpose_major_programmes_and_delivery_organisation + overall_primary_purpose_safety_licensing_and_regulation_body + log_staffing_fte_inpost + overall_classification_crown_ndpb + overall_classification_executive_agency + overall_classification_non_ministerial_department + overall_classification_tribunal_ndpb, data = LS22_23_encoded)

qqPlot(model3) #Normality
vif(model3) #Multicollinearity (IV Independence)
plot(predict(model3), residuals(model3)) #Homoscedasticity
durbinWatsonTest(model3)

model4 <- lm(log_finance_budget_overall ~ staffing_pct_fte_london + overall_primary_purpose_cultural_institution + overall_primary_purpose_economic_or_sectoral_regulator + overall_primary_purpose_grant_and_subsidy_issuing_body + overall_primary_purpose_justice_prosecutorial_and_enforcement + overall_primary_purpose_major_programmes_and_delivery_organisation + overall_primary_purpose_safety_licensing_and_regulation_body + log_staffing_fte_inpost + overall_classification_crown_ndpb + overall_classification_executive_agency + overall_classification_non_ministerial_department + overall_classification_tribunal_ndpb, data = no_top_ten)
tab_model(
    model3, model4,
    dv.labels = c("Model 3: Top Ten incl", "Model 4: Top Ten Removed"))
exp(1.73)

qqPlot(model4) #Normality
vif(model4) #Multicollinearity (IV Independence)
plot(predict(model4), residuals(model4)) #Homoscedasticity
durbinWatsonTest(model4)

#Visualisations ----

ggplot(LS22_23_encoded, aes(y = sum(finance_budget_overall), x = overall_parent_department)) +
    geom_bar(position = "stack", stat="identity") +
    ggtitle("Total Budget per Department") +
    xlab("Department") +
    ylab("Total Budget (£)")

ggplot(no_na_data, aes(x=staffing_fte_inpost, y=finance_budget_overall, color=overall_classification, main("Figure 1: Scatter plot of ALB Budget and FTE")+
                           xlab("Total Staff (FTE)")+
                           ylab("Total ALB Budget (£)"))) +
    geom_point()

Sum_budget_by_dept <- no_na_data %>%
    group_by(overall_classification) %>% 
    summarise(sum_budget = sum(finance_budget_overall, na.rm = TRUE))

Sum_budget_by_dept$Classification <- NA
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Advisory NDPB'] <- 'NDPB'
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Crown NDPB'] <- 'NDPB'
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Executive NDPB'] <- 'NDPB'
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Tribunal NDPB'] <- 'NDPB'
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Executive Agency'] <- 'Executive Agency'
Sum_budget_by_dept$Classification[Sum_budget_by_dept$overall_classification == 'Non-Ministerial Department'] <- 'Non-Ministerial Department'

ggplot(Sum_budget_by_dept, aes(x=Classification, y=sum_budget)) + 
    geom_bar(stat = "identity")

Sum_FTE_by_dept <- no_na_data %>%
    group_by(overall_classification) %>% 
    summarise(sum_FTE = sum(staffing_fte_inpost, na.rm = TRUE))

Sum_FTE_by_dept$Classification <- NA
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Advisory NDPB'] <- 'NDPB'
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Crown NDPB'] <- 'NDPB'
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Executive NDPB'] <- 'NDPB'
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Tribunal NDPB'] <- 'NDPB'
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Executive Agency'] <- 'Executive Agency'
Sum_FTE_by_dept$Classification[Sum_FTE_by_dept$overall_classification == 'Non-Ministerial Department'] <- 'Non-Ministerial Department'

ggplot(Sum_FTE_by_dept, aes(x=Classification, y=sum_FTE)) + 
    geom_bar(stat = "identity")

coeff <- 100000
sum_class <- left_join(Sum_budget_by_dept, Sum_FTE_by_dept, by = "Classification")%>%
    mutate(sum_budget = sum_budget/coeff) %>% 
    pivot_longer(cols = c("sum_FTE", "sum_budget")) 
    

ggplot(sum_class, aes(fill=name, y=value, x=Classification)) +
    scale_y_continuous(labels =
                           label_number(scale_cut = cut_short_scale()),
                       
                       name = "Total FTE",
                       
                       sec.axis = sec_axis(~.*coeff, name="Total Government Funding (£)", labels =
                                               label_number(scale_cut = cut_short_scale()))
    ) +
    ggtitle("Figure X: Total FTE and Budget per Classification in 2022/23") +
    geom_bar(position="dodge", stat="identity")


plot_ly(
    x = no_na_data$finance_budget_overall,
    y = no_na_data$overall_classification ,
    name = "Budget per Purpose",
    type = "bar")

ggplot(LS22_23_encoded, aes(x=log_staffing_fte_inpost, y=log_finance_budget_overall)) + 
    geom_point() + 
    ggtitle("Scatter plot of ALB Budget and FTE") + 
    xlab("Staff (FTE)") + 
    ylab("Budget (£)")
