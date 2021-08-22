#Import Data
get.Import.Data <- function(file.realcov, file.return, file.lable){
  lable_sector <- readxl::read_xlsx(file.lable)
  lable <- as.matrix(t(lable_sector[1]))
  list.realcov <- readMat(file.realcov)
  data.realcov <- list.realcov$R
  
  return <- readMat(file.return)
  return <- return$r
  
  df.date <- as.data.frame(t(list.realcov$DATE.R))
  colnames(df.date) <- "date"
  df.date$date <- as.Date(as.character(df.date$date), "%Y%m%d")
  
  data <- list(R = return, Cov = data.realcov, date = df.date, lable = lable, sector = lable_sector)
  return(data)
}
