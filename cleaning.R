#This is a script to define the functions that take the input data and clean it for model building
# Install packages and mask functions --------------------
rm(list=ls())
list.of.packages <- c("plyr","dplyr","readxl","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
select <- dplyr::select
rm(list.of.packages,new.packages)
# Functions to be used --------------------
monthTotals <- function( filepath ){
  
}