keep <- c("overall_organisation", "overall_organisation")
top10_check <- LS22_23[keep]

no_top_ten <- LS22_23_encoded
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "NHS England"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "Education and Skills Funding Agency"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "HM Revenue and Customs"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "Network Rail Limited"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "UK Research and Innovation"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "High Speed 2 Ltd"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "National Highways"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "Homes and Communities Agency (Homes England)"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "HM Prison and Probation Service"] <- NA
no_top_ten$overall_organisation[no_top_ten$overall_organisation == "Nuclear Decommissioning Authority"] <- NA

no_top_ten <- no_top_ten |> 
    filter(!is.na(overall_organisation))