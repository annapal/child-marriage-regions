
create_meta_data <- function(merged_data, iso3) {
  
  # Get country dataset
  data <- merged_data[[iso3]]
  
  # Create wb
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = iso3)
  
  # Make headers bold
  boldHeaderStyle <- createStyle(fontColour = "black", textDecoration = "bold")
  addStyle(wb, iso3, style = boldHeaderStyle, rows = 1, cols = 1:11)
  
  # Get different surveys
  surveys <- unique(data$surveyid)
  writeData(wb, iso3, c("survey", surveys),
            colNames=FALSE, xy=c(1,1))
  writeData(wb, iso3, c("n", as.vector(table(data$surveyid))),
            colNames=FALSE, xy=c(2,1))
  addStyle(wb, iso3, style = boldHeaderStyle, rows = 1:(length(surveys)+1), cols = 1)
  
  # Set column names
  writeData(wb, iso3, c("sample"),
            colNames=FALSE, xy=c(3,1))
  writeData(wb, iso3, c("interview year"),
            colNames=FALSE, xy=c(4,1))
  writeData(wb, iso3, c("birth years"),
            colNames=FALSE, xy=c(5,1))
  writeData(wb, iso3, c("proportion birth year missing"),
            colNames=FALSE, xy=c(6,1))
  writeData(wb, iso3, c("proportion marriage year missing"),
            colNames=FALSE, xy=c(7,1))
  writeData(wb, iso3, c("previous region available"),
            colNames=FALSE, xy=c(8,1))
  writeData(wb, iso3, c("Reg"),
            colNames=FALSE, xy=c(9,1))
  writeData(wb, iso3, c("Adm1"),
            colNames=FALSE, xy=c(10,1))
  writeData(wb, iso3, c("Adm2"),
            colNames=FALSE, xy=c(11,1))
  
  # Write in the data for each survey
  for (i in 1:length(surveys)) {
    data_subs <- subset(data, surveyid==surveys[i])
    writeData(wb, iso3, data_subs$samp[1],
              colNames=FALSE, xy=c(3,i+1))
    writeData(wb, iso3, min(data_subs$int_y),
              colNames=FALSE, xy=c(4,i+1))
    writeData(wb, iso3, paste0(min(data_subs$birth_y), ", ", max(data_subs$birth_y)),
              colNames=FALSE, xy=c(5,i+1))
    writeData(wb, iso3, sum(data_subs$birth_imp%in%c(7,8))/nrow(data_subs),
              colNames=FALSE, xy=c(6,i+1))
    writeData(wb, iso3, sum(data_subs$mar_imp%in%c(7,8))/nrow(data_subs),
              colNames=FALSE, xy=c(7,i+1))
    writeData(wb, iso3, any(!is.na(data_subs$prev_reg)),
              colNames=FALSE, xy=c(8,i+1))
    writeData(wb, iso3, any(!is.na(data_subs$Reg)),
              colNames=FALSE, xy=c(9,i+1))
    writeData(wb, iso3, any(!is.na(data_subs$Adm1)),
              colNames=FALSE, xy=c(10,i+1))
    writeData(wb, iso3, any(!is.na(data_subs$Adm2)),
              colNames=FALSE, xy=c(11,i+1))
  }
  
  # Save workbook
  saveWorkbook(wb, paste0("data/merged/meta_data/", iso3, ".xlsx"),
               overwrite = TRUE)
}
