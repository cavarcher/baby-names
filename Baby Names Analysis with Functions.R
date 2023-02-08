load_data <- function(url){
  #Download the baby names .zip from Social Security Administration
  download.file(url, "babynames.zip")
  unzip("babynames.zip")
}

build_data_frame <- function(firstYear, lastYear){
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
  #I did this here to avoid using an attribute[5] with a vector[0]
  colnames(total_names_by_year) <- c("name","sex","count","year", "rank")
  
  return(total_names_by_year)
}

# Return a vector of each year's deviation from the total median
delta_from_median_vector <- function(names, v_name, v_sex){
  # Filter the data so it's only for the name and sex specified
  filtered_list <- names %>%
    filter(name==v_name & sex==v_sex)
  
  #Find the median count
  med <- summarize(filtered_list, m = median(rank)) %>% 
        pull(m)
  #print(summarize(filtered_list, m = median(rank)))
  std <- summarize(filtered_list, s = sd(rank)) %>% 
    pull(s)
  #print(summarize(filtered_list, s = sd(rank)))
  
  med_deltas <- data.frame()
  
  #Find vector with each delta
  #for (x in 1:nrow(filtered_list)){
    #should check for each year in range instead of each row in filtered list
    #if no name present, count=0 so rank=max rank from that year+1
  #  med_deltas <- rbind(med_deltas, (filtered_list[x,5]-med)/std)
  #}
  #Find vector with each delta
  #should check for each year in range instead of each row in filtered list
  #if no name present, count=0 so rank=max rank from that year+1
  for (i in begin_year:end_year){
    #filter for only the current year's row
    #should return either one or zero rows
    year_list <- filtered_list[filtered_list$year==i,]
    #If the list has more then 0 rows, we have a rank for that year and can
    #evaluate the difference from median
    if(nrow(year_list)>0){
      med_deltas <- rbind(med_deltas, (year_list[1,5]-med)/std)
      #print(paste(v_name,"delta is",(year_list[1,5]-med)/std,"in",i))
      #print(year_list)
    }
    
    #If it has zero rows, that name did not appear on the SSA's data for that
    #that year. Set the rank to the max rank that year+1 and evaluate
    else{
      max_rank <- nrow(filter(names, year==i))
      #print(paste("Max Rank:",max_rank))
      #print(paste("Median:",med))
      #print(paste("SD:",std))
      med_deltas <- rbind(med_deltas, (max_rank+1-med)/std)
      #print(paste(v_name,"not found in",i,"delta set to",(max_rank+1-med)/std))
    }
  }
  
  return (med_deltas)
}

# Takes two delta vectors and returns the sum of the difference. 
# The smaller the number, the closer the name rank
vector_difference <- function(name_a, name_b){
 return(sum(abs(name_a - name_b)))
} 

compare_names <- function(total_names_by_year, first_name, first_sex, second_name, second_sex){
  total_names_by_year %>%
    filter((name==first_name & sex==first_sex) | (name==second_name & sex==second_sex)) %>%
      ggplot()+
      geom_point(mapping=aes(x=year, y=rank, color=name))+
      geom_line(mapping=aes(x=year, y=rank, color=name))+
      labs(title=paste("Rank of",first_name,"and",second_name))+
      scale_color_manual(values = c("blue","red"))+
      scale_y_reverse()+
      facet_wrap(~name, scales ="free")
}

find_closest_names <- function(ssa_data, all_names, target_name, target_sex){
  #Find the list of the target name's deviation from the median
  target_name_delta <- delta_from_median_vector(ssa_data, target_name, target_sex)
  # Filters out the target name (and the target name of the opposite sex)
  filtered_name_list <- filter(all_names, name!=target_name)
  
  #new data frame for the result after the for loop
  result_list <- data.frame()
  #iterate through each name and find the delta number.
  #add to a data frame with name, sex and delta number
  last_percent <- -1
  for(i in 1:nrow(filtered_name_list)){
    #percent complete print out
    percent <- round(i/nrow(filtered_name_list)*100)
    if(percent>last_percent){
      print(paste(percent,"% completed"))
      last_percent <- percent
    }
    
    #error handling for names that do not occur in every year
    #need to improve handling here 
    tryCatch(
      {
        name_i <- filtered_name_list[i,1]
        sex_i <- filtered_name_list[i,2]
        delta_i <- delta_from_median_vector(ssa_data, name_i, sex_i)
        diff_i <- vector_difference(target_name_delta, delta_i)
        result_list <- rbind(result_list, c(name_i, sex_i, as.double(diff_i)))
      },
      error=function(cond){
        #Should not happen with corrected delta_from_median_vector function
        print(paste("Dropping ",name_i,sep=""))
      }
    )
  }
  colnames(result_list) <- c("name", "sex", "sameness")
  result_list <- result_list[order(as.double(result_list$sameness)),]
  return(result_list)
}

#code
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

#variables
url <- "https://www.ssa.gov/oact/babynames/names.zip"
target_name <- "Rose"
target_sex <- "F"
begin_year <- 1950
end_year <- 2000
#download data from the SSA website
load_data(url)
#build the data frame with all names between the two years
#columns will be name, sex, count, year and rank
ssa_data <- build_data_frame(begin_year, end_year)
rownames(ssa_data) <- NULL

write.csv(ssa_data, "Social_Security_Names_by_Year.csv", row.names=TRUE)
unique_names <- unique(ssa_data[c("name", "sex")])
#unique_names <- unique_names[order(unique_names$name),]

diff_sort_list <- find_closest_names(ssa_data,
                                     unique_names,
                                     target_name,
                                    target_sex)
male_diff_sort_list <- filter(diff_sort_list, sex=="M")
female_diff_sort_list <- filter(diff_sort_list, sex=="F")
for (i in 1:10){
  name_i <- male_diff_sort_list[i,1]
  sex_i <- male_diff_sort_list[i,2]
  print(paste("Comparing",target_name,"and",name_i))
  print(
    ggplot(filter(ssa_data,(name==target_name & sex==target_sex) | (name==name_i & sex==sex_i)))+
    geom_point(mapping=aes(x=year, y=rank, color=name))+
    geom_line(mapping=aes(x=year, y=rank, color=name))+
    labs(title=paste("Rank of",target_name,"and",name_i))+
    scale_color_manual(values = c("blue","red"))+
    scale_y_reverse()+
    facet_wrap(~name, scales ="free")
    )
}

test_set <- delta_from_median_vector(ssa_data, "Finley", "F")
