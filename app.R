#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shinyWidgets)
library(bslib)
library(shiny)
library(tidyverse)
library(sjlabelled)
library(labelVector)
library(shinyjs)
library(plotly)
theme_set(theme_classic(base_size = 15))
setwd
LS22_23 <- readxl::read_xlsx("KM Data Slice/2024-08-08 KM Landscape Data Slice.xlsx")
TS_Data <- readxl::read_xlsx("KM Data Slice/2024-06-21 -LIVE- Consolidated PBD and CS Workforce Data.xlsx")
options(scipen = 999)
TS_no_na_data <- TS_Data |> 
    filter(!is.na(fte)) # remove rows of NAs in fte
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
TS_model1 <- lm(log(fte) ~ time + classification + parent_department, data = TS_no_na_data)
TS_model2 <- lm(log(finance_budget_govt_funding) ~ time + classification + parent_department, data = TS_no_na_data)
# Define UI for application that draws a histogram
source("22_23_ALB_Budget_and_FTE.R")
LS22_23_vis <- no_na_data
LS22_23_vis$staffing_fte_inpost[LS22_23_vis$staffing_fte_inpost < 0] <- NA
LS22_23_vis$finance_budget_overall[LS22_23_vis$finance_budget_overall < 10] <- NA
LS22_23_vis$finance_spend_rdel_overall[LS22_23_vis$finance_spend_rdel_overall == 0] <- NA
LS22_23_vis$staffing_fte_employed_in_london[LS22_23_vis$staffing_fte_employed_in_london == 0] <- NA

dept_choices <- unique(LS22_23$overall_parent_department)
class_choices <- unique(LS22_23$overall_classification)
coeff <- 1000000

# User Interface ----------------------------------------------------------

ui <- navbarPage(# Application title
    
    titlePanel("ALB budget and FTE relationship"),
    tabPanel("Homepage",
             mainPanel(p("Introduction to Dashboard"),
                       p("This dashboard serves as a way for users to interpret the 2022/23 Landscape analsysis and use it to inform themselves of the trends in budget and FTE not only over time but specifically looking at the ALBs captured in the 2022/23 financial year")
             )),

# ALB scatter and barplot -------------------------------------------------
    tabPanel("ALB budget and FTE relationship for financial year 2022/23",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Staff",
                        "Amount of FTE",
                        min = 0,
                        max = 70000,
                        value = c(0,2000)),
            sliderInput("Budget",
                        "Budget Value",
                        min = 0,step = 25000000,
                        max = 20000000000,
                        value = c(0,200000000)),
            pickerInput(inputId = "department_Drop_Down",label = "Select a Department",
                        choices = sort(dept_choices), options = list('actions-box' = TRUE, `live-search` = TRUE, `selected-text-format` = 'count > 1'), selected = dept_choices, multiple = TRUE),
            selectInput(inputId = "Colour",
                        label = "Select Colour Variable",
                        choices = as.list(c("Purpose", "Classification")))
        ),

        # Show a plot of the generated distribution
        mainPanel(navset_card_underline(title = "Scatterplot of ALB Budget and FTE",
           nav_panel("Scatterplot", plotlyOutput("user_data_plot"),
                     p("Scatterplot shows the realtionship between ALB Budget and FTE is positively correlated")), 
           nav_panel("Data Table",DT::dataTableOutput("ALB_Budget_Model")),
           
           mainPanel(navset_card_underline(title = "Stacked plot of ALB Budget and FTE per purpose type",
                                           nav_panel("Budget", plotOutput("barplot1", width = "100%"),
                                                     p(" "),
                                                     p("The above Barplot splits all the ALBs in the 2022/23 Landscape Analysis and splits them by purpose type, showing their respective total budget per purpose type")),
                                           nav_panel("FTE", plotOutput("barplot2", width = "100%"),
                                                                       p(" "),
                                                                       p("The above Barplot splits all the ALBs in the 2022/23 Landscape Analysis and splits them by purpose type, showing their respective total FTE per purpose type"))))
        )
    )
)),

# Regression assumption plots ---------------------------------------------

tabPanel("2022/23 Regression Model",
         sidebarLayout( sidebarPanel( 
             pickerInput(inputId = "model_picker", 
                         label = "Select a Model:", 
                         choices = c("Model 1", "Model 2", "Model 3", "Model 4"), 
                         selected = "Model 1", 
                         multiple = FALSE ), 
             pickerInput( inputId = "diagnostic_picker", 
                          label = "Select Diagnostic Plot:", 
                          choices = c("Linearity", "QQ Plot", "Homoscedasticity"), 
                          selected = "Linearity", 
                          multiple = FALSE ) ), 
             mainPanel( plotOutput("selected_plot"),
                        p(" "),
                        p("The above visuals are the proof of assumptions being met."),
                        p("The QQ-plot shows normality, if normality was not met the line would be majorly shifted and not represent the line of best fit at all."),
                        p("Linearity, shows linearity of the two main variables of the model, if there was no linearity when the using contiuous variables the line would not be straight nor would it show positive correlation"),
                        p("Homoscedasticity, shows that the error variance is constant across the values of the dependent variable within the model, there should be recognisable shapes within the plot such as a cone")
                        )) ),
tabPanel("Budget and FTE changes across time in ALBs",
         sidebarPanel(
             sliderInput("time",
                         "Year",
                         sep = "",
                         min = 2014,
                         max = 2023,
                         value = c(2014, 2023))
         ),
         mainPanel(navset_card_underline(title = "Line Plot of ALB FTE and Government Funding Over Time",
                                         nav_panel("Budget", plotlyOutput("Budgetlineplot"),
                                                   p(" "),
                                                   p("The above area plot shows budget across the period of time that was captured in the time-series landscape dataset, showing the gradual increases with the spike in funding during the Covid-19 Pandemic")),
                                         nav_panel("FTE", plotlyOutput("FTElineplot"),
                                                   p(" "),
                                                   p("The above area plot shows changes in FTE across the time-series landscape dataset, showing more gradual and controlled increases since 2017 with less major spikes as seen in the budgetary data")),
                                         nav_panel("Combined FTE and Budget", plotlyOutput("combinedlineplot"),
                                                   p("The above plot shows the relationship between FTE and ALB Budget over time across all ALBs. The relationship is correlated aside from a major spike in budget in 2021 caused by the inflated demand on Public Services during the Covid-19 Pandemic")))
                   )),
tabPanel("Time-Series Regression Model",
         mainPanel(navset_card_underline(title = "Regression Model assumption plots",
                                nav_panel("Linearity", plotOutput("TSLinearity1")),
                                nav_panel("Normality",plotOutput("TS_QQ_1")))
                   )),
tabPanel("Methodology",
         mainPanel(p("This dashboard was created as a supplementary piece to the report on the study of the relationship between FTE and budget of an ALB"),
                   p("In order to analyse the relationship between budget and FTE of an ALB for the financial year 2022/23, this study will use the 22/23 Landscape (LS) Dataset created by the Benchmarking and Reform Analysis Unit (BRAU) within The Public Bodies team in The Cabinet Office. This dataset captures a large range of data on each ALB details of each ALB overseen by the cabinet office, collecting information regarding the finances, employment, chair and any other relevant information to the overseeing of a particular ALB. The information within the LS Dataset is provided by the ALBs themselves as they were tasked by members of the BRAU team to fill in a commission with the correct information directly by the team. 
"),
                   p("For the time-series analysis of the relationship between FTE and ALB budget the dataset this study will be using is a dataset created by combining the public bodies directory (PBD) , civil service workforce datasets from the years 2010 to 2021 and the LS Dataset. This combined dataset is called the Time-Series Landscape (TSLS). This was created in collaboration with another member of the BRAU team, who had the FTE data already combined on a time-series scale with the financial data from the PBD datasets was still to be added in order for the analysis of this study to be calculated. This data was required to be extensively cleaned as it went back 14 years, with varying levels of data literacy being applied to the collection of the data. Each year's PBD dataset for FTE and financial information will be pooled into one central dataset in order for the time-series analysis to be conducted and patterns or trends in the data to be gleaned. 

Following cleaning, the financial columns were added to the requisite datasets before being fed into a name standardising pipeline. Created by the BRAU team, the standardiser catches occasions where an ALB either changes names or is entered into the dataset with a different format. This is important as different people from the ALB may have entered the same ALB in a different format, for example being abbreviated instead of spelt out in its entirety. This must occur for ALBs to be recognised as the same body otherwise the time-series element of the study would be skewed and unreliable as the different spellings of an ALB would be treated as a completely different ALB. 
")
         )),
tabPanel("Concluding Statement",
         mainPanel(p("This dashboard was created in order to make the 2022/23 Landscape Analysis accessible and interpretable for users. In the future as more financial years' landscape is captured the dashboard can be updated to create a time-series that not only has each years' analysis but also a more thorough time-series with a wider variety of variables available."),
                   p("The research conducted here is the first step in being able to analyse a government's goals and agendas to see if they have achieved or completed their stated aims. This can also act as a basis of policy advice to ministers and policymakers to assign budgets on an evidence-based approach with an expansion of the dashboard."))))

# Server ------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
    user_data <- reactive({
        data <- LS22_23 %>% 
            filter(overall_parent_department %in% input$department_Drop_Down) %>% 
            filter(between(staffing_fte_inpost, left = input$Staff[1],
                           right = input$Staff[2])) %>% 
            filter(between(finance_budget_overall, left = input$Budget[1],
                           right = input$Budget[2])) %>%
            select(ALB = overall_organisation, 
                   Department = overall_parent_department, 
                   Staff = staffing_fte_inpost, 
                   Budget = finance_budget_overall, 
                   Expenditure = finance_spend_rdel_overall, 
                   Classification = overall_classification, 
                   Purpose = overall_primary_purpose)
        
        print("Dimensions of user_data:")
        print(dim(data))
        print("Summary of user_data:")
        print(summary(data))
        print("First few rows of user_data:")
        print(head(data))
        
        data
    })
# ScatterPlot -------------------------------------------------------------
   
     output$user_data_plot <- renderPlotly({
        static_FTE_Budget_Scatter <- user_data() %>% 
            ggplot(aes(x = Staff, y = Budget)) +
            geom_point(aes(fill = get(input$Colour), 
                           text = paste0("ALB: ", ALB, 
                                         "<br> Department: ", Department,
                                         "<br> Staff: ", Staff)), size=4, alpha = 0.8, shape = 21 )+
            scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
            scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
             labs(fill = input$Colour)
        
        ggplotly(static_FTE_Budget_Scatter, tooltip = "text")
    })
    
    output$ALB_Budget_Model <- DT::renderDataTable({
       user_data()     
    })

# Barplot for FTE/Budget --------------------------------------------------
    output$barplot1 <- renderPlot({
        data <- user_data()
       ggplot(data, aes(x = Budget, y = Purpose)) +
           geom_bar(stat = "identity", aes(fill = get(input$Colour),
                                            text1 = paste0("Purpose: ", Purpose,
                                                           "<br> Staff: ", Staff))) +
           scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
            labs(fill=input$Colour) +
           theme(legend.position = if_else(input$Colour == "Purpose", "none", "bottom"))
    })
    
    output$barplot2 <- renderPlot({
        data <- user_data()
        ggplot(data, aes(x = Staff, y = Purpose)) +
            geom_bar(stat = "identity", aes(fill = get(input$Colour))) +
            scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
            labs(fill=input$Colour) +
            theme(legend.position = if_else(input$Colour == "Purpose", "none", "bottom"))
    })
    
    my_data_year_sum_fte2 <- reactive({TS_no_na_data %>%
            group_by(time) %>% 
            filter(between(time, left = input$time[1],
                           input$time[2])) %>%# or use year here if variable is called year
            summarise(sum_fte = sum(fte, na.rm = TRUE))    
    })
    
    my_data_year_sum_budget2 <- reactive({TS_no_na_data %>% 
            group_by(time) %>% 
            filter(between(time, left = input$time[1],
                           input$time[2])) %>% 
            summarise(sum_budget = sum(finance_budget_govt_funding, na.rm = TRUE))
                      })
    
    sums <- reactive({TS_no_na_data %>% 
        group_by(time) %>%
            filter(between(time, left = input$time[1],
                           input$time[2])) %>% 
            summarise(sum_fte = sum(fte, na.rm = TRUE), sums_budget = sum(finance_budget_govt_funding, na.rm = TRUE)) %>% 
            mutate(sums_budget = sums_budget/coeff) %>% 
                pivot_longer(cols = c("sum_fte", "sums_budget"))
        })

# FTE TS Lineplot ---------------------------------------------------------
    output$FTElineplot <- renderPlotly({ 
        data <- my_data_year_sum_fte2()
        plot_ly(data, x = ~time, y = ~sum_fte, type = 'scatter', mode = 'none', fill = 'tozeroy') 
    })
    output$Budgetlineplot <- renderPlotly({ 
        data <- my_data_year_sum_budget2()
        plot_ly(data, x = ~time, y = ~sum_budget, type = 'scatter', mode = 'none', fill = 'tozeroy') 
    })
    
    
     output$combinedlineplot <- renderPlotly({
        
        data <- sums()
        
        static_compare <- ggplot(data, aes(x = time)) +
            geom_line(aes(y = value, group = name, color = name)) + 
            scale_y_continuous(
                labels = label_number(scale_cut = cut_short_scale()),
                name = "Total FTE",
                sec.axis = sec_axis(~.*coeff, 
                                    name = "Total Government Funding (£)")) +
            ggtitle("Total FTE and Budget over time")
        
        ggplotly(static_compare)
    })

    output$selected_plot <- renderPlot({  
        req(input$model_picker, input$diagnostic_picker) 
        model <- input$model_picker 
        diagnostic <- input$diagnostic_picker 
         
        if (model == "Model 1") { 
            if (diagnostic == "Linearity") { 
                ggplot(LS22_23_encoded, aes(x=log_staffing_fte_inpost, y=log_finance_budget_overall)) + 
                    geom_point() + 
                    ggtitle("Scatter plot of ALB Budget and FTE") + 
                    xlab("Staff (FTE)") + 
                    ylab("Budget (£)") } 
            
            else if (diagnostic == "QQ Plot") { 
                qqPlot(model1, main = "QQ Plot of 22/23 regression model 1") } 
            else if (diagnostic == "Homoscedasticity") { 
                plot(predict(model1), residuals(model1), 
                     main = "Homoscedasticity of Model 1", 
                     xlab = "Predicted Values", 
                     ylab = "Residuals")
            } 
            }  
        else if (model == "Model 2") { 
            if (diagnostic == "Linearity") { 
                ggplot(no_top_ten, aes(x=log_staffing_fte_inpost, y=log_finance_budget_overall)) + 
                    geom_point() + 
                    ggtitle("Scatter plot of ALB Budget and FTE") + 
                    xlab("Staff (FTE)") + 
                    ylab("Budget (£)") } 
            
            else if (diagnostic == "QQ Plot") { 
                qqPlot(model2, main = "QQ Plot of 22/23 regression model 2") } 
            else if (diagnostic == "Homoscedasticity") { 
                plot(predict(model2), residuals(model2), 
                     main = "Homoscedasticity of Model 2", 
                     xlab = "Predicted Values", 
                     ylab = "Residuals")
            } 
        }  
        else if (model == "Model 3") { 
            if (diagnostic == "Linearity") { 
                ggplot(LS22_23_encoded, aes(x=log_finance_budget_overall, y=staffing_pct_fte_london)) + 
                    geom_point() + 
                    ggtitle("Scatter plot of ALB Budget and London based FTE") + 
                    xlab("Budget (£)") + 
                    ylab("Percentage of FTE based in London") } 
            
            else if (diagnostic == "QQ Plot") { 
                qqPlot(model3, main = "QQ Plot of 22/23 regression model 3") } 
            else if (diagnostic == "Homoscedasticity") { 
                plot(predict(model3), residuals(model3), 
                     main = "Homoscedasticity of Model 3", 
                     xlab = "Predicted Values", 
                     ylab = "Residuals")
            }
        }
        else if (model == "Model 4") { 
            if (diagnostic == "Linearity") { 
                ggplot(no_top_ten, aes(x=log_finance_budget_overall, y=staffing_pct_fte_london)) + 
                    geom_point() + 
                    ggtitle("Scatter plot of ALB Budget and London based FTE") + 
                    xlab("Budget (£)") + 
                    ylab("Percentage of FTE based in London") } 
            
            else if (diagnostic == "QQ Plot") { 
                qqPlot(model4, main = "QQ Plot of 22/23 regression model 4") } 
            else if (diagnostic == "Homoscedasticity") { 
                plot(predict(model4), residuals(model4), 
                     main = "Homoscedasticity of Model 4", 
                     xlab = "Predicted Values", 
                     ylab = "Residuals")
            }
        }
        })
    output$TSLinearity1 <- renderPlot({ggplot(TS_no_na_data, aes(x=time, y=finance_budget_govt_funding, main("Scatter plot of ALB Budget and FTE")+
                                                                       xlab("Year")+
                                                                       ylab("Budget(£)"))) +
            geom_point()
    }) 
    output$TS_QQ_1 <- renderPlot({qqPlot(TS_model1, main = "QQ Plot of Time-Series regression model 1")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
