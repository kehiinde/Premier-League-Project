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

homes_Data = data.frame(additional_info$Home) |>



full_data = bind_rows(data_list)


#Clean data 
colSums(is.na(full_data))
full_data$Date <- as.Date(full_data$Date, format="%d/%m/%Y") 
full_data = full_data |> select(-c('B365H', "B365D", "B365A",
                "BSH", 'BSD', "BSA", 'BWH', "BWD", 'BWA'))


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
full_data = full_data|> mutate(Goal_Diff = FTHG - FTAG)

check_Home = function(x,y){
  if(y == 'H'){
    return(1)
  } else{
    return(0)
  }
}


full_data = full_data |> mutate(isHome = ifelse(FTR == 'H', 1, 0))

full_data = full_data |> group_by(HomeTeam) |>
  arrange(Date) |> mutate(AvgGoalsLast5 = zoo::rollmean(FTHG,
                        k=5, fill= NA, align = "right"))
   


