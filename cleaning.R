#This is a script that takes the input data from excel and cleans it for model building, 
#outputting a .csv to be passed to Python
# Install packages and mask functions --------------------
rm(list=ls())
list.of.packages <- c("plyr","dplyr","purrr","readxl","reshape","stringr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))
select <- dplyr::select
filter <- dplyr::filter
rm(list.of.packages,new.packages)
#The following option increases the memory allocation for loading Excel file
options( java.parameters = "-Xmx4g" )
# Functions to be used --------------------
listify <- function(x){
  #This function reads the Excel workbook, storing each sheet as a data.frame
  readxl::read_excel(path = filepath, sheet = x, col_names = T)
}
cleanColNames <- function (x){
  #The purpose of this function is to keep all column names, but set "domain"
  #as the first column name in all sheets, for later reference.
  colnames(x) <- c("domain",colnames(x)[2:ncol(x)])
  return(x)
}
cleanDomainNAs <- function(x){
  #This function gets rid of the empty rows that were erroneously imported by readxl;
  #Those rows were imported as rows of entire NAs, so filter where first column != NA
  y <- as.data.frame(x) %>% filter( !is.na( domain ) ) 
  return(y)
}
cleanDataTypes <- function(x){
  #Because of the few "Data Not Found" entries in the spreadsheet, the ostensible numeric
  #rows are imported as characters. Fix this in the non-name columns, return as data.frame
  y <- map_at(.x = x, .at=2:ncol(x), .f=as.numeric) %>% data.frame
  return(y)
}
# Cleaning the Excel sheets to data.frames --------------------
filepath = paste0(getwd(),"/jan2016report.xlsx")
sheets <- readxl::excel_sheets(filepath)
#Create a list of dataframes from the list of sheet names, minus the last sheet which requires some tidyr-ing
dfs <- llply(.data = sheets[-length(sheets)], .fun=listify) %>% 
  llply(., .fun=cleanColNames) %>% 
  llply(.,data.frame) %>% 
  llply(.,cleanDomainNAs) %>% 
  llply(.,cleanDataTypes) %>% 
  llply(., unique) 

#The last dataframe, traffic: names are changed for easier reference
traffic <- listify(sheets[8])
names(traffic) <- c("site","source",'jan','feb','mar')
#The following fills NAs in the site column with replicates of the values that ARE populated, then pivots the dataframe on 
#source so that each source is its own column. Moreover, row order is preserved as a factor
traffic %>% tidyr::fill(site) %>% dcast(factor(site,levels=unique(site))~source,value.var='jan') 
# Creating training data from data.frames --------------------
#Need to multiply entire rows of data frames together; so drop the first and last columns and do element-wise 
#multiplication, summing the rows
multiplicand = dfs[[1]][,-c(1,ncol(dfs[[1]]))]
multipliers = llply(.data = dfs[c(2,3,4)], .fun=function(x){return(x[,-c(1,ncol(dfs[[1]]))])})

#The below variables has first column of the total visits per site; then the product of total_pages_per_visit*total_visits to get
#total pages; total_avg_visit_duration*total_visits to get avg_duration for each site; total_bounce_rate*total_visits to get 
#bounce rate for each domain; then the site names are prepended to the front; then the global rank variable is appended, after 
#assigning missing ranks the value -1; then the percentage desktop visits is created with mutate from the total visits and 
#desktop visits columns

features <- cbind(rowSums(multiplicand) %>% unname, sapply( multipliers, function(x){return( rowSums(multiplicand * x) %>% unname )} ) )%>% 
  data.frame %>% 
  cbind(dfs[[1]] %>% select(1),.) %>% 
  cbind(., d = rowSums(dfs[[6]][,-c(1,ncol(dfs[[6]]))]) %>% unname ) %>% 
  cbind(., dfs[[5]] %>% select(2) %>% replace_na(., list("X2016.01"=-1))) %>%
  mutate(.,p = d/X1) %>%
  rename(c("domain" = "domain","X1" = "total_visits", "X2"="pages_per_visit", "X3"="avg_visit_duration","X4" = "bounce_rate", "d" = "desktop_visits","X2016.01" = "global_rank","p"="desktop_percent"))
# Formatting target variable from csv ----------
y <- read.csv( file= paste0(getwd(), "/revenue.csv") ) %>%
  map_at(.,1,function(x){factor(str_trim(str_replace(x,"www.",""), "both"))}) %>%
  data.frame
#There are some duplicated sites in the revenue .csv, so get the unique then merge 
full_data <- merge(features, y[unique(y$domain),], by=intersect(names(features),names(y)) )
# Write the cleaned data to a .csv ----------
write.csv(features, file = "training_data.csv",row.names=F)