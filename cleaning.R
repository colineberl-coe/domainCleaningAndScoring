#This is a script to define the functions that take the input data and clean it for model building
# Install packages and mask functions --------------------
rm(list=ls())
list.of.packages <- c("plyr","dplyr","readxl","reshape","stringr","tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = TRUE))
select <- dplyr::select
rm(list.of.packages,new.packages)
options( java.parameters = "-Xmx4g" )
# Functions to be used --------------------
listify <- function(x){readxl::read_excel(path = filepath, sheet = x, col_names = T)}
cleanColNames <- function (x){
    colnames(x) <- c("domain",colnames(x)[2:ncol(x)])
    return(x)
  }
naify <- function(x){return( as.data.frame(x) %>% filter( !is.na( domain ) ) )}

# Logic of the function --------------------
filepath = paste0(getwd(),"/jandata.xlsx")
sheets <- readxl::excel_sheets(filepath)
#Create a list of dataframes from the list of sheet names, minus the last sheet which requires some tidyr-ing
dfs <- llply(.data = sheets[-length(sheets)], .fun=listify) %>% llply(., .fun=cleanColNames) %>% llply(.,data.frame) %>% llply(.,naify)

#The last dataframe, traffic: names are changed for easier reference
traffic <- listify(sheets[8])
names(traffic) <- c("site","source",'jan','feb','mar')
#The following fills NAs in the site column with replicates of the values that ARE populated, then pivots the dataframe on 
#source so that each source is its own column. Moreover, row order is preserved as a factor
traffic %>% fill(site) %>% dcast(factor(site,levels=unique(site))~source,value.var='jan')

#
