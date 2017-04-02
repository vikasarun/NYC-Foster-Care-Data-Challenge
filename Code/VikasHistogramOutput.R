setwd("C:/Users/Vikas/Google Drive/NYC Foster Care Data Challenge")
# Change the above directory to the foster care data challenge folder on your computer

HistogramOutput = function()
{
  column_num_list = c(10:43,47:49,88:94,100:102) # all child variables
  data = read.csv("./Clean Data/afcars_foster_clean.csv")
  names_of_columns = colnames(data)[column_num_list]
  num_na = rep(NA, length(column_num_list))
  for(i in 1:length(column_num_list))
  {
    num_na[i] = sum(is.na(data[,names_of_columns[i]]))
    filename_string = paste("./Figures/",names_of_columns[i],"Histogram.JPG")
    jpeg(filename = filename_string)
    hist(as.numeric(data[,names_of_columns[i]]), xlab = names_of_columns[i])
    dev.off()
  }
  data_out = cbind(names_of_columns, column_num_list, num_na)
  write.csv(data_out, "./Figures/NACounts.csv", row.names = FALSE)
}


