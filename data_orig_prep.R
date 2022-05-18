library(tidyverse)
library(readxl)

data_folder = "D:\\phd\\jump load\\data\\"
file_name = "Raw Jump Data.xlsx"
sheets = excel_sheets(paste0(data_folder, file_name))

# read all sheets in file
load_sheets = sheets[which(str_detect(sheets, "[[:alpha:]]-\\d"))]
d_original = data.frame()
for(i in load_sheets){
  temp_data = read_excel(paste0(data_folder, file_name), sheet = i)
  d_original = rbind(d_original, temp_data)
  d_original
}
d_original
names(d_original)

baseline = sheets[!sheets %in% load_sheets]
d_baseline = read_excel(paste0(data_folder, file_name), sheet = baseline, skip = 2)
