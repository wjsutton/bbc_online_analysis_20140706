# Author: Will Sutton
# Date: 5th August 2018

# The following script:
# 1. loads the original data set.
# 2. fixes some data issues.
# 3. calculates pages entries, exits, and browser sessions.
# 4. calculates time spent per pages views.
# 5. estimates time spent where one cannot be calculated by taking the average time spent.
# 6. packages all the enhancements as a new data set.

#setwd("D:/Tableau Reports/20180727 - AB BBC Online Project")
#source("process.R")

.libPaths("D:/R/R-3.3.1/library")

library(data.table)
library(dplyr)
library(sqldf)
library(stringr)
library(mice)
#library(feather)
options(digits = 15)
memory.limit(size=50000)

### Loading data
data <- fread(file="hashedOutput.csv",
              colClasses = c(double(),character(),double()
                             ,date(),double(),character(),character()
                             ,character(),character(),character()
                             ,character(),character(),character()),
              fill=T)

### Correcting some issues with the data
# Case 1 numbers loaded in as 1,234e+12
# Case 2 data that has been shifted by the csv 

# Solving Case 1...
e_cases <- sqldf("SELECT V1,
                 [UTC.timestamp]
                 FROM data 
                 WHERE [UTC.timestamp] LIKE '%e%'")

digi <- substr(e_cases$UTC.timestamp,1,str_locate(e_cases$UTC.timestamp,'e')[,1]-1)
power <- substr(e_cases$UTC.timestamp,str_locate(e_cases$UTC.timestamp,'e')[,1]+2,nchar(e_cases$UTC.timestamp))

replacements <- data.frame(V1=e_cases$V1,
                           UTC.timestamp=e_cases$UTC.timestamp, 
                           UTC.timestamp_new=str_pad(gsub(',','',digi),as.double(power)+1,side= c("right"), pad ='0'))

data <- sqldf("SELECT
              D.V1
              ,D.Browsers
              ,COALESCE(R.[UTC.timestamp_new],D.[UTC.timestamp]) as 'UTC.timestamp'
              ,D.[Day]
              ,D.[Hour]
              ,D.[Entry.type]
              ,D.[Page]
              ,D.[Page.URL]
              ,D.[Previous.page]
              ,D.[Virtual.site]
              ,D.[Region]
              ,D.[Platform]
              ,D.[Application.type]
              FROM data AS D
              LEFT JOIN replacements AS R ON D.V1=R.V1")


# Solving Case 2...
shifted_entries <- data %>% filter(Application.type=="1")

page_url <- substr(shifted_entries$Page.URL,1,str_locate(shifted_entries$Page.URL,'"\"')[,1])
previous_page  <- gsub(' ','',substr(shifted_entries$Page.URL,str_locate(shifted_entries$Page.URL,'"\"')[,2]+1,nchar(shifted_entries$Page.URL)))
virtual_site <- gsub('[\\|\"]','',shifted_entries$Previous.page)

shifted_entries_corrected <- data.frame(shifted_entries[,1:7]
                                        ,Page.URL=page_url
                                        ,Previous.page=previous_page
                                        ,Virtual.site=virtual_site
                                        ,Region=shifted_entries$Virtual.site
                                        ,Platform=shifted_entries$Region
                                        ,Application.type=shifted_entries$Platform)

correct_data <- data %>% filter(Application.type!="1")
data <- rbind(correct_data,shifted_entries_corrected)

# Data issues solved.
# removing temporary files to save memory...
rm(list=c('e_cases','digi','power','replacements','shifted_entries','page_url',
          'previous_page','virtual_site','shifted_entries_corrected','correct_data'))
print("Data issues solved.")

### Caluating BBC page entries, sessions and exits
# the span from entry to exit is considered a session
# sessions create a window for more accurate calculation of time spent
# over calculating the first and last page visited per browser

first_entry <- data %>% filter(is.na(data$Previous.page))
first_entry <- first_entry[,1]
first_entry <- data.frame(V1=first_entry,first_page=1, stringsAsFactors = F)

browsers_sessions <- data[,1:3]
browsers_sessions <- left_join(browsers_sessions,first_entry, by ="V1")

session <- browsers_sessions %>% filter(first_page==1)
session <- session %>% group_by(Browsers) %>% mutate(session = 1:n())

max_sesh <- sqldf("SELECT Browsers, MAX(session) as max_session FROM session GROUP BY Browsers")
single_sessions <- max_sesh %>% filter(max_session==1)
single_sessions <- single_sessions[,1, drop=FALSE]

multi_sessions <- max_sesh %>% filter(max_session!=1)
multi_sessions <- multi_sessions[,1, drop=FALSE]

multi_sessions <- inner_join(session,multi_sessions, by = "Browsers")
multi_sessions2  <- data.frame(multi_sessions,session_lookup=multi_sessions$session +1)

session_span <- sqldf("SELECT 
                  A.V1
                  ,A.Browsers
                  ,A.[UTC.timestamp]
                  ,B.[UTC.timestamp] as end_utc
                  ,A.first_page
                  ,A.session FROM multi_sessions2 AS A 
                  INNER JOIN multi_sessions AS B ON A.session_lookup=B.session 
                  AND A.Browsers=B.Browsers")

last_session <- sqldf("SELECT 
                      M.* 
                      FROM multi_sessions AS M
                      LEFT JOIN session_span AS T ON T.V1=M.V1
                      WHERE T.V1 IS NULL")


single_sessions <- sqldf("SELECT
                    T.*
                    ,1 as session
                    FROM browsers_sessions AS T
                    INNER JOIN single_sessions AS S 
                    ON S.Browsers=T.Browsers")

multi_sessions <- sqldf("SELECT
                    T.*
                    ,S.session
                    FROM browsers_sessions AS T
                    INNER JOIN session_span AS S 
                    ON S.Browsers=T.Browsers
                    AND T.[UTC.timestamp]>=S.[UTC.timestamp]
                    AND T.[UTC.timestamp]<S.end_utc")

last_session <- sqldf("SELECT
                    T.*
                   ,S.session
                   FROM browsers_sessions AS T
                   INNER JOIN last_session AS S 
                   ON S.Browsers=T.Browsers
                   AND T.[UTC.timestamp]>=S.[UTC.timestamp]")

sessions_merged <- rbind(single_sessions,multi_sessions,last_session)

# Cases where the session started the previous day
missing_session <- sqldf("SELECT 
                D.V1
                ,D.Browsers
                ,D.[UTC.timestamp]
                ,NULL as first_page
                ,0 as session
                FROM data AS D
                LEFT JOIN sessions_merged AS T ON T.V1=D.V1
                WHERE T.V1 IS NULL")

sessions_merged <- rbind(sessions_merged,missing_session)

exit_page <- sqldf("WITH CTE AS 
                  (SELECT 
                   Browsers
                   ,session 
                   ,MAX([UTC.timestamp]) as [UTC.timestamp] 
                   FROM sessions_merged
                  GROUP BY Browsers, session 
                   )
                   SELECT 
                   v1
                   ,'1' as exit_page
                   FROM sessions_merged AS T
                   INNER JOIN CTE ON CTE.Browsers=T.Browsers
                   AND CTE.session=T.session
                   AND CTE.[UTC.timestamp]=T.[UTC.timestamp]")

sessions_merged <- left_join(sessions_merged,exit_page, by ="V1")

subset <- data[,c(1,4:13)]
data <- inner_join(sessions_merged,subset, by="V1")

print("sessions, entry and ext points found.")
# sessions, entry and ext points found.
# removing temporary files to save memory...
rm(list=c('first_entry','browsers_sessions','session','max_sesh','single_sessions',
          'multi_sessions','multi_sessions2','session_span','last_session',
          'sessions_merged','missing_session','exit_page','subset'))



# calculate time per session

id_and_factors <- data[,c(1,8,9,13:16)]

time_calc <- data[,c(1,2,5,3)]
#test <- time[,c(1:2,9)]
time_calc <- time_calc %>% arrange(Browsers, session,UTC.timestamp) %>% group_by(Browsers,session) %>% mutate(row_number = 1:n())
time_calc2 <- data.frame(time_calc[,1:4],time_lookup=time_calc$row_number-1)
time_calc2 <- sqldf("SELECT T2.*, 
               T3.[UTC.timestamp] as compare_time
               FROM time_calc AS T2 
               LEFT JOIN time_calc2 AS T3 ON T2.Browsers=T3.Browsers 
               AND T2.session=T3.session 
               AND T2.row_number=T3.time_lookup")
null_times <- time_calc2 %>% filter(is.na(time_calc2$compare_time))
null_times <- data.frame(null_times[,1:3],time=NA)
times <- time_calc2 %>% filter(is.na(time_calc2$compare_time)==FALSE)
times <- data.frame(times[,1:3],time=(as.double(times$compare_time) - as.double(times$UTC.timestamp)))

times_merged <- rbind(times,null_times)

times_df <- sqldf("SELECT 
                       S.V1
                       ,S.Browsers
                       ,S.session
                       ,T.[Entry.type]
                       ,T.[Hour]
                       ,T.[Virtual.site]
                       ,T.[Region]
                       ,T.Platform
                       ,T.[Application.type]
                       ,S.time
                       FROM id_and_factors AS T
                       INNER JOIN times_merged AS S ON T.V1=S.V1")

rm(list=c('id_and_factors','time_calc','time_calc2','times','null_times','times_merged'))
print("times found")

### predicting null times

init <- mice(times_df, maxit=0)
meth <- init$method
predM <- init$predictorMatrix

predM[, c("V1","Browsers","session")] <- 0

meth[c("V1","Browsers","session","Entry.type","Hour"
       ,"Virtual.site","Region","Platform","Application.type")] <- ""
meth[c("time")] <- "mean"

times_predicted <- mice(times_df, method=meth, predictorMatrix = predM, m=5)
times_predicted <- complete(times_predicted)

print("missing times predicted")

time_lookup <- times_predicted[,c(1,10)]

rm(list=c('init','predM','times_df','times_predicted','meth'))

data <- inner_join(data,time_lookup,by = "V1")

print("dataset made.")
