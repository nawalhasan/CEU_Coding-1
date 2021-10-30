##########################
##     HOMEWORK         ##
##      CLASS 2         ##
##       CEU            ##
##########################
library( tidyverse )
library(tidyr)
library(dplyr)


# 0) Clear work space
rm( list = ls() )



# 1) Load both data from github page and inspect (summary,glimpse)
#   Hint: you will need the `raw.github...` url address

schools <- read.csv( "https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_schools.csv" )
scores <- read.csv( "https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_scores.csv" )

summary( schools )
glimpse( schools )

summary( scores )
glimpse( scores )

# 2) Merge the two data table into one called `df`

df <- left_join( scores , schools, by = "district" )

# 3) Put the district variable into numeric format

as.numeric( df$district )

##checking
is.numeric( df$district )

# 4) Create two new variables from `school_county`: 
#     - school should contain only the name of the school - character format
#     - county should only contain the name of the county - factor format

df <- separate( df , school_county , " - " ,
                into = c("school","county") )

##already in character format
is.character( df$school ) 

##changed to factor format
df <- mutate( df , county = factor( county ) )
is.factor( df$county )

# 5) Find missing values, write 1 sentence what and why to do what you do and execute.
# as they seems to be completly random, we can drop these observations


# To extract the missing values from each table I used the following function. 
# In many cases being aware of missing data is crucial and ignoring them can seriously impair your analysis.
# However in this case as these values are radom I omit them from our table

table(df$district, useNA = 'ifany')
table(df$school, useNA = 'ifany')
table(df$county, useNA = 'ifany')
table(df$students, useNA = 'ifany')
table(df$teachers, useNA = 'ifany')
table(df$english, useNA = 'ifany')
table(df$read,useNA = "ifany")
table(df$math, useNA = "ifany")

# dropping missing rows from the data frame
df <- na.omit(df)


# 6) Create a new variable called `score` which averages the english, math and read scores


df <- mutate(df, score = rowMeans(df[c('math', 'english', 'read')]))

# 7) Find the county which has the largest number of schools in the data 
#     and calculate the average and the standard deviation of the score.

head(arrange(count(group_by(df,county)),desc(n)),1)

df %>% 
  filter(county == "Sonoma") %>% group_by(county) %>%
  summarise(mean=mean(score))

df %>% 
  filter(county == "Sonoma") %>% group_by(county) %>%
  summarise(sd=sd(score))

