# Author: Will Sutton
# Date: 5th August 2018

# Creates the right handside chart for the Tableau visualisation "BBC_Online_20140706.twbx"
# it is a "select what you like" scatter chart 
# for users to do their own analysis
# creates aggregated view for all factors: entry, hour, product, region, platform, application
# with additional metrics, such as number of sessions, number of pages viewed

#setwd("D:/Tableau Reports/20180727 - AB BBC Online Project")
#source("create_explore_chart.R")

id <- c(1:6)
factors<- c("entry","hour","product","region","platform","application")
columns <- c("Entry.type","Hour","Virtual.site","Region","Platform","Application.type")
sql_columns <- c("[Entry.type]","Hour","[Virtual.site]","Region","Platform","[Application.type]")

loop_df <- data.frame(id,factors,columns,sql_columns)

for(i in 1:length(loop_df$id)){
  assign(
    paste0(loop_df$factors[i],"_by_browser"),
    unique(data[,c("Browsers",paste0(loop_df$columns[i]))])
  )
  assign(
    paste0(loop_df$factors[i],"_metrics_by_browser"),
    sqldf(paste0(
              "SELECT ",loop_df$sql_column[i],",
              Browsers,
              COUNT(DISTINCT [Virtual.site]) as product_count,
              SUM(time) as time,
              COUNT(DISTINCT Browsers) as browser_count,
              COUNT(DISTINCT [Page.URL]) as page_count,
              COUNT(DISTINCT session) as session_count
              FROM data 
              GROUP BY Browsers, ",loop_df$sql_column[i]))
  )
  
  
  assign(
    paste0(loop_df$factors[i],"_sql"),
    sqldf(
          paste0("SELECT 
          D.",loop_df$sql_column[i],",
          SUM(M.product_count) as product_count,
          SUM(M.time) as time,
          SUM(M.browser_count) as browser_count,
          SUM(M.page_count) as page_count,
          SUM(M.session_count) as session_count
          FROM ",loop_df$factors[i],"_by_browser AS D
          INNER JOIN ",loop_df$factors[i],"_metrics_by_browser AS M ON M.Browsers=D.Browsers
          AND M.",loop_df$sql_column[i],"=D.",loop_df$sql_column[i],"
          GROUP BY D.",loop_df$sql_column[i]))
  )

  assign(
    paste0(loop_df$factors[i]),
    data.frame(breakdown=eval(parse(text = paste0(loop_df$factors[i],"_sql[,1]"))),
               reach_kpi=eval(parse(text = paste0(loop_df$factors[i],"_sql$browser_count"))),
               use_kpi=(eval(parse(text = paste0(loop_df$factors[i],"_sql$time")))/eval(parse(text = paste0(loop_df$factors[i],"_sql$browser_count")))),
               breadth_kpi=(eval(parse(text = paste0(loop_df$factors[i],"_sql$product_count")))/eval(parse(text = paste0(loop_df$factors[i],"_sql$browser_count")))),
               eval(parse(text = paste0(loop_df$factors[i],"_sql[,2:6]"))))
  )

  assign(
    paste0(loop_df$factors[i],"_reach"),
    data.frame(kpi_filter='reach',
               kpi_value=eval(parse(text = paste0(loop_df$factors[i],"$reach_kpi"))),
               chart=paste0(loop_df$factors[i]),
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"$breakdown"))),
               eval(parse(text = paste0(loop_df$factors[i],"[,4:9]"))))
  )
  
  assign(
    paste0(loop_df$factors[i],"_use"),
    data.frame(kpi_filter='use',
               kpi_value=eval(parse(text = paste0(loop_df$factors[i],"$use_kpi"))),
               chart=paste0(loop_df$factors[i]),
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"$breakdown"))),
               eval(parse(text = paste0(loop_df$factors[i],"[,4:9]"))))
  )
  
  assign(
    paste0(loop_df$factors[i],"_breadth"),
    data.frame(kpi_filter='breadth',
               kpi_value=eval(parse(text = paste0(loop_df$factors[i],"$breadth_kpi"))),
               chart=paste0(loop_df$factors[i]),
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"$breakdown"))),
               eval(parse(text = paste0(loop_df$factors[i],"[,4:9]"))))
  )
  
  assign(
    paste0(loop_df$factors[i],"_explore"),
    rbind(eval(parse(text = paste0(loop_df$factors[i],"_reach"))),
          eval(parse(text = paste0(loop_df$factors[i],"_use"))),
          eval(parse(text = paste0(loop_df$factors[i],"_breadth"))))
  )
  
}
full_explore <- rbind(
  entry_explore,
  hour_explore,
  product_explore,
  region_explore,
  platform_explore,
  application_explore
)
