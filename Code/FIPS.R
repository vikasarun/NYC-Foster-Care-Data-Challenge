setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Foster Care")

library(dplyr)
FIPS_data = as.data.frame(read.csv("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", header = FALSE))
colnames(FIPS_data) = c ("State", "StateFIPS", "County.FIPS", "CountyName", "FIPSClass")
FIPS_data = FIPS_data[FIPS_data$State == "NY",]
FIPS_data$County.FIPS = formatC(FIPS_data$County.FIPS, width = 3, format = "d", flag = 0)
CombinedFIPS = rep(NA, nrow(FIPS_data))
for(i in 1:nrow(FIPS_data))
{
  CombinedFIPS[i] = paste(FIPS_data$StateFIPS[i], FIPS_data$County.FIPS[i], sep = "")
}
FIPS_data$TotalFIPS = as.character(CombinedFIPS)
ZIP_data = read.csv("./Raw Data/New_York_State_ZIP_Codes-County_FIPS_Cross-Reference.csv")
ZIP_data = ZIP_data[,c("County.Name", "County.FIPS", "ZIP.Code")]
colnames(ZIP_data) = c("County.Name", "TotalFIPS","ZIP.Code")
ZIP_data$TotalFIPS = as.character(ZIP_data$TotalFIPS)
total_data = left_join(FIPS_data, ZIP_data)
total_data = total_data[,c("State", "CountyName", "TotalFIPS", "County.Name", "ZIP.Code")]
write.csv(total_data, file = "./Clean Data/ZIPandFIPS.csv")