library(tidyverse)
# Assessment: Combining Tables

#................Question 1..........................

# You have created data frames tab1 and tab2 of state population and 
# election data, similar to our module videos:

tab1x<-c("Alabama","Alaska","Arizona","Delaware","District of Columbia")
tab1y<-c(4779736,710231,6392017,897934,601723)
tab1q<-data.frame(state=tab1x,population=tab1y)
tab1q
tab1q<-as_tibble(tab1q)
tab1q
tab2x<-c("Alabama","Alaska","Arizona","California","Colorado", "Connecticut")
tab2y<-c(9,3,11,55,9,7)
tab2q<-data.frame(state=tab2x,population=tab2y)
tab2q<-as_tibble(tab2q)
tab2q
dim(tab1q)
# [1] 5 2
dim(tab2q)
# [1] 6 2
# What are the dimensions of the table dat, created by
# the following command?
dat <-left_join(tab1q, tab2q, by = "state")
dat
dim(dat)
# Answer: 5 rows by 3 columns

#................Question 2..........................

# We are still using the tab1 and tab2 tables shown in question 1. What join command would create a new table "dat" with three rows and two columns?

dat_inner <- inner_join(tab1q, tab2q, by = "state") 
dat_inner
dim(dat_inner)
dat_semi <-semi_join(tab1q, tab2q, by = "state") 
dat_semi 
dim(dat_semi)
dat_full <-full_join(tab1q, tab2q, by = "state") 
dat_full
dim(dat_full)
dat_right <-right_join(tab1q, tab2q, by = "state") 
dat_right
dim(dat_right)
# Answer: dat <- semi_join(tab1, tab2, by = "state") 

#................Question 3..........................

# Which of the following are real differences between the join and bind functions?
              # Answer
# Binding functions combine by position, while join functions match by variables.
# Joining functions can join datasets of different dimensions, but the bind functions must match on the appropriate dimension (either same row or column numbers).
# Bind functions can combine both vectors and dataframes, while join functions work for only for dataframes.

#................Question 4..........................

# We have two simple tables, shown below, with columns x and y:
Obj1<-c("a","b")
Obj2<-c("a","a")
df1<-data.frame(x=Obj1,y=Obj2)
df1
df2<-data.frame(x=Obj2,y=Obj1)
df2
# Which command would result in the following table?
# final
     # x     y    
     # b     a 
final_df2_df1 <- setdiff(df2, df1)
final_df2_df1
final_df1_df2 <- setdiff(df1, df2)
final_df1_df2
# Answer: final <- setdiff(df1, df2)
# Introduction to Questions 5-7
# Install and load the Lahman library. 
# This library contains a variety of datasets related to US professional baseball. 
# We will use this library for the next few questions and will discuss 
# it more extensively in the Regression course. For now, focus on 
# wrangling the data rather than understanding the statistics.
# The Batting data frame contains the offensive statistics for all baseball players over several seasons.  
# Filter this data frame to define top as the top 10 home run (HR) hitters in 2016:
install.packages(Lahman)
library("Lahman")
str(Batting)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
top
# Also Inspect the Master data frame, which has demographic information for all players:
Master %>% as_tibble()
str(Master)
str(top)
#................Question 5..........................

# Use the correct join or bind function to create a combined table of the names 
# and statistics of the top 10 home run (HR) hitters for 2016. This table should
# have the player ID, first name, last name, and number of HR for 
# the top 10 players. Name this data frame top_names.
     # Identify the join or bind that fills the blank in this code to create the correct table:

# top_names <- top %>% ___________________ %>%
 # select(playerID, nameFirst, nameLast, HR)

top_names <- top %>% left_join(Master,top,by = "playerID")%>%
  select(playerID, nameFirst, nameLast, HR)
top_names
# Answer : left_join(Master)

#................Question 6..........................

# Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, 
# then use the correct bind join function to add a salary column 
# to the top_names data frame from the previous question.
# Name the new data frame top_salary. Use this code framework:

#top_salary <- Salaries %>% filter(yearID == 2016) %>%
#  ______________ %>%
#  select(nameFirst, nameLast, teamID, HR, salary)

str(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary
# Answer: right_join(top_names)

#................Question 7..........................
# Inspect the AwardsPlayers table. 
str(AwardsPlayers)
# Filter awards to include only the year 2016.
AwardsPlayers_2016<-AwardsPlayers%>%
  filter(yearID==2016)
dim(AwardsPlayers_2016)
# How many players from the top 10 home run hitters won at least one award in 2016?
top10_AwardsPlayers_2016<-AwardsPlayers_2016%>%
  inner_join(top_names)
top10_AwardsPlayers_2016
atleast1_top10_AwardsPlayers_2016<-data.frame(table(top10_AwardsPlayers_2016$playerID))
dim(top10_AwardsPlayers_2016)
dim(atleast1_top10_AwardsPlayers_2016)
# Answer 3

Awarded_2016_nottop10<-AwardsPlayers_2016%>%
  anti_join(top_names)%>%
  arrange(playerID)
Awarded_2016_nottop10
atleastone<-distinct(Awarded_2016_nottop10)
atleastone_count<-data.frame(table(atleastone$playerID))
dim(atleastone_count)  
# Answer 44