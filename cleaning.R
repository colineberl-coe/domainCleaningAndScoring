#This is a script to define the functions that take the input data and clean it for model building
# Install packages and mask functions --------------------
rm(list=ls())
list.of.packages <- c("plyr","dplyr","readxl","stringr","XLConnect")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))
select <- dplyr::select
rm(list.of.packages,new.packages)
options( java.parameters = "-Xmx4g" )
# Functions to be used --------------------
xcelToDB <- function( filepath ){
  #Use the XLConnect to load the workbook and get the list of sheets
  w <- XLConnect::loadWorkbook(filepath)
  sheets <- XLConnect::getSheets(w)
  #Defining utility function to return list of dataframes, one entry per sheet 
  listify <- function( x ){readxl::read_excel(path = filepath, sheet = x, col_names = T)}
  #Defining utility function to remove NAs from list of dataframes
  naify <- function( x ){as.data.frame(x) %>% filter(!is.na(colnames(x)[1]))}
  d <- sapply(sheets, listify)
  return( sapply(d, naify))
}
