# FTE-Budget-Project.R
This repo contains all of the scripts created in order to read in and analyse the 22/23 Landscape Analysis and the time-series dataset for analysis for the FTE and Budget project

# 00a_Install_Nec_Libs.R

This script should be ran when a user is first using the repo to install the packages used in the main scripts to the users rstudio. 

# 00_Import_Nec_Libs.R
This is a simple script that contains all the basic libraries used within the two mains scripts 

# 01_NA_Removal.R

This scripts makes outliers in the 22/23 script NA and removes the NAs specifically from the variables in question as when the necessary variables are log transformed NAs create errors within the models

# 02_One-Hot_Encoding.R

This script one-hot encodes the nominal control variables for the 22/23 analysis and writes them to a new dataset so the original is not affected.

# 02.5_TS_One-Hot_Encoding.R

This script one-hot encodes the nominal control variables for the time-series analysis and writes them to a new dataset so the original is not affected.

# 03_Top_Ten_Removal.R

This script removes the top ten ALBs in terms of funding from the 22/23 dataset, to run the models without them in order to see if the findings are present across all the ALBs. 

# 22_23_ALB_Budget_and_FTE.R

This is the main script for the data analysis for the 2022/23 Landscape Analysis, containing all the models ran with their assumptions and all the visualisations from the report. This script uses the codes prior to this in the repo in order to complete the tasks that they were built for and make the code more readable.

# TS_Data_Analysis.R

This is the main script for the data analysis for the time-series analysis, this similar to the 22/23 script contain the code for all the models run in order to conduct the analysis for the the report. It also creates visualisations used in both the final report and dashboard which were the two outputs of this project.

# App.R

This is the Shiny script that creates the interactive dashboard which is one of the projects main outputs. It contains visualisations of the key variables that BRAU may be able to utilise and update in the future. 

# Visualisation.R

This script was created in order to make visualisations and remove the outliers within the variables so they were readable and interpretable within the report. 
