#GROUP 10
#HW2
#1
df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))
aggregate(df1$Sales, by=list(df1$State), FUN=sum)
library(dplyr)
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

#2 

df = read.csv("~/Downloads/WorldCupMatches.csv", header=T)
head(df)

#(b)
summary(df)

#(a)
nrow(df)
ncol(df)

#(c)
length(unique(df$City))

#(d)
mean(df$Attendance, na.rm = TRUE)

#(e)
aggregate(df$Home.Team.Goals)
aggregate(df$Home.Team.Goals, by=list(df$Home.Team.Name), FUN=sum)

#(f)
df %>% group_by(Year) %>% summarise(avg_attendance = mean(Attendance, na.rm = TRUE))

#3

df2 = read.csv("~/Downloads/metabolite.csv", header=T)
summary(df2)
nrow(df2)
nrow(df2)
ncol(df2)
head(df2)

#Find how many Alzheimers patients there are in the data set. (Hint: Please refer to question 1)
df2 %>%
  filter(Label == "Alzheimer") %>%
  summarise(count = n())

#Determine the number of missing values for each column. (Hint: is.na( ) 
colSums(is.na(df2))

#Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame. (Hint: is.na( ) 
df3 <- df1[!is.na(df2$Dopamine), ]

#In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same column. (Hint: there is median( ) function.
median_value <- median(df3$c4.OH.Pro, na.rm = TRUE)
df3$c4.OH.Pro[is.na(df2$c4.OH.Pro)] <- median_value

