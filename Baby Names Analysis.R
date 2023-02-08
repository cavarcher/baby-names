install.packages("dplyr")
library(dplyr)

#Download the baby names .zip from Social Security Administration
url <-
  "https://www.ssa.gov/oact/babynames/names.zip"
download.file(url, "babynames.zip")
unzip("babynames.zip")

#Variables
firstYear <- 1880
lastYear <- 1880

#Create an empty data frame:
total_names_by_year <- data.frame()

#Read each file from 1880 to 2021
for (x in firstYear:lastYear){
  
  # reads the initial table in from the .txt
  # 3 columns: name, sex and count. 
  # need to add year and then add onto total data frame
  # source files are ordered by sex (F then M) and then by count desc
  year_names <- read.table(paste("yob",x,".txt",sep=""), 
                           header=FALSE, 
                           sep=",")
  
  #create a vector that is the same length as the data frame with the year in each row
  year_vector <-rep(c(x),each=nrow(year_names))
  
  #add the year as the fourth column
  year_names <- cbind(year_names, year_vector)
  
  #name the columns
  names(year_names) <- c("name", "sex", "count", "year")
  
  #order the names by popularity
  year_names <- year_names[ order(-year_names$count),]
  
  #add rank as the fifth column
  year_names <- year_names %>%
    mutate(rank = row_number())

  #Add the final data frame for the year onto total_names_by_year
  total_names_by_year <- rbind(total_names_by_year,year_names)
}

#name the columns in total_names_by_year
#I did this here to avoid using an attribute[5] witha vector[0]
colnames(total_names_by_year) <- c("name","sex","count","year", "rank")

#sample plot
first_name <- "Adam"
first_sex <- "M"
second_name <-"Rose"
second_sex <- "F"
total_names_by_year %>%
  filter((name==first_name & sex==first_sex) | (name==second_name & sex==second_sex)) %>%
  ggplot()+
    geom_point(mapping=aes(x=year, y=rank, color=name))+
    geom_line(mapping=aes(x=year, y=rank, color=name))+
    labs(title=paste("Rank of",first_name,"and",second_name))+
    scale_color_manual(values = c("blue","red"))+
    scale_y_reverse()+
    facet_wrap(~name, scales ="free")
