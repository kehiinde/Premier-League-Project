#Read files
library(tidyverse)
library(dplyr)
year_2000 = read.csv("2000-01.csv")
year_2001 = read.csv("2001-02.csv")
year_2002 = read.csv("2002-03.csv")
year_2003 = read.csv("2003-04.csv")
year_2004 = read.csv("2004-05.csv")
year_2005 = read.csv("2005-06.csv")
year_2006 = read.csv("2006-07.csv")
year_2007 = read.csv("2007-08.csv")
year_2008 = read.csv("2008-09.csv")
year_2009 = read.csv("2009-10.csv")
year_2010 = read.csv("2010-11.csv")
year_2011 = read.csv("2011-12.csv")
year_2012 = read.csv("2012-13.csv")
year_2013 = read.csv("2013-14.csv")
year_2014 = read.csv("2014-15.csv")
year_2015 = read.csv("2015-16.csv")
year_2016 = read.csv("2016-17.csv")
year_2017 = read.csv("2017-18.csv")
year_2018 = read.csv("2018-19.csv")
year_2019 = read.csv("2019-20.csv")
year_2020 = read.csv("2020-2021.csv")
year_2021 = read.csv("2021-2022.csv")



#join data
data_list = list(year_2000, year_2001, year_2002, year_2003,
                 year_2004, year_2005, year_2006, year_2007,
                 year_2008, year_2009, year_2010, year_2011,
                 year_2012, year_2013, year_2014, year_2015,
                 year_2016, year_2017, year_2018, year_2019,
                 year_2020, year_2021)





full_data = bind_rows(data_list)


#Clean data 
colSums(is.na(full_data))
full_data$Date <- as.Date(full_data$Date, format="%d/%m/%Y") 
full_data = full_data |> select(-c('B365H', "B365D", "B365A",
                "BSH", 'BSD', "BSA", 'BWH', "BWD", 'BWA'))

full_data$Attendance[is.na(full_data$Attendance)] =
  median(full_data$Attendance, na.rm = TRUE)

full_data = full_data |> select(where(~all(!is.na(.))))



#EDA
#win rate per teams, over the years 
full_data |> group_by(HomeTeam) |>
  summarise(WinRate = mean(FTR == 'H')) |> arrange(desc(WinRate))

full_data |> 
  ggplot(aes(x = FTHG, y = FTAG)) +
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  labs(x = "Home Goals", y = "Away Goals", title = "Home vs Away Goals")


#Feature engineering
library(zoo)
full_data = full_data|> mutate(Goal_Diff = FTHG - FTAG)

full_data = full_data |> mutate(isHome = ifelse(FTR == 'H', 1, 0))

full_data <- full_data |> 
  group_by(HomeTeam) |> 
  mutate(
    AvgGoalsLast5 = zoo::rollmean(FTHG, k = 5, fill = NA, align = "right"),
    AvgGoalsLast5 = ifelse(is.na(AvgGoalsLast5), cummean(FTHG), AvgGoalsLast5)
  )


full_data = full_data |>arrange(Date) |>
  mutate(Win_streak = 
           cumsum(lag(FTR, default = "L") == 'H' & FTR == 'H'))

full_data <- full_data |>
  arrange(Date) |>
  group_by(HomeTeam) |>
  mutate(
    HomePoints = lag(cumsum(case_when(
      FTR == "H" ~ 3,  
      FTR == "D" ~ 1,  
      FTR == "A" ~ 0,  
      TRUE ~ 0
    )), default = 0)  # Shift cumulative sum so it only considers past games
  ) |>
  ungroup() |>
  group_by(AwayTeam) |>
  mutate(
    AwayPoints = lag(cumsum(case_when(
      FTR == "H" ~ 0,  
      FTR == "D" ~ 1,  
      FTR == "A" ~ 3,  
      TRUE ~ 0
    )), default = 0)
  ) |>
  ungroup()





#----Build the predictive model
library(caret)

# Convert categorical variables to factors
full_data <- full_data |> 
  mutate(
    HomeTeam = as.factor(HomeTeam),
    AwayTeam = as.factor(AwayTeam),
    Referee = as.factor(Referee),
    Div = as.factor(Div)
  )
full_data$FTR = as.factor(full_data$FTR)  # Convert to categorical


# Fill NA values with median or mode
full_data$Attendance[is.na(full_data$Attendance)] =
  median(full_data$Attendance, na.rm = TRUE)

# Normalize numeric features
normalize = function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
num_cols = c("Goal_Diff", "Win_streak", "AvgGoalsLast5", "HomePoints", "AwayPoints")
full_data[num_cols] = lapply(full_data[num_cols], normalize)


set.seed(150)
split = createDataPartition(full_data$FTR, p=0.8, list = FALSE)
train_data = full_data[split, ]
test_data = full_data[-split, ]

library(randomForest)

# Train a Random Forest model
train_data$FTR <- factor(train_data$FTR)

my_model <- randomForest(FTR ~ Win_streak + AvgGoalsLast5 + HomePoints + AwayPoints, 
                         data = train_data, ntree = 100, importance = TRUE)

# Make Predictions
predictions <- predict(my_model, test_data)

# Check Accuracy
confusionMatrix(predictions, test_data$FTR)






