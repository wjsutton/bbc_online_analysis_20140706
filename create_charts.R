# Author: Will Sutton
# Date: 5th August 2018

# The following script prepares the individual charts for the Tableau visualisation "BBC_Online_20140706.twbx"
# The reason for this is to keep the size of the data inserted into Tableau as efficient as possible.
# So Tableau's job is to visual the data, not having to calculate KPIs etc.

# Three primary KPIs have been derived:
# 1. Reach:  number of distinct Browsers
# 2. Use: Average Time a browser spends on BBC Online
# 3. Breadth: Average number of BBC products viewed by browsers

# The left hand side charts are:
# a KPI table which filters all other charts by that KPI
# a product breakdown which filters all other charts (except the explore) by a BBC product

# The middle charts evaluate the factors:
# Entry.type, Hour, Region, Platform, Application.type
# by all KPIs and BBC products
# this a selected by a highlight table (hlt)
# region has all been assigned a position on a hex map

# The right hand side chart is a "select what you like" scatter chart 
# for users to do their own analysis
# this is described in "create_explore_chart.R"

#setwd("D:/Tableau Reports/20180727 - AB BBC Online Project")
#source("create_charts.R")

kpi_reach <- length(unique(data$Browsers))
kpi_use <- sqldf("SELECT Browsers, SUM(time) as time FROM data GROUP BY Browsers")
kpi_use <- mean(kpi_use$time)
kpi_breadth <- sqldf("SELECT Browsers, COUNT(DISTINCT [Virtual.site]) as product_count FROM data GROUP BY Browsers")
kpi_breadth <- mean(kpi_breadth$product_count)

kpi_filter <- c('reach','use','breadth')
chart_label <- c('Reach: total unique browsers','Use: total time spent','Breadth: average amount of products visited')
values <- c(kpi_reach,kpi_use,kpi_breadth)

kpis <- data.frame(chart='kpis',
                   subchart='kpis',
                   hlt_filter=NA,
                   kpi_filter=kpi_filter,
                   chart_label=chart_label,
                   breakdown=NA,
                   value=values,
                   stringsAsFactors = F)

product_reach <- sqldf("SELECT [Virtual.site], COUNT(DISTINCT Browsers) as browser_count FROM data GROUP BY [Virtual.site]")
names(product_reach) <- c("breakdown","value")
product_reach <- data.frame(kpi_filter='reach',chart_label='product',product_reach, stringsAsFactors = F)

product_use <- sqldf("SELECT Browsers, [Virtual.site], SUM(time) as time FROM data GROUP BY Browsers, [Virtual.site]")
product_use <- sqldf("SELECT [Virtual.site], AVG(time) as time FROM product_use GROUP BY [Virtual.site]")
names(product_use) <- c("breakdown","value")
product_use <- data.frame(kpi_filter='use',chart_label='product',product_use, stringsAsFactors = F)

product_dist_by_browser <- sqldf("SELECT DISTINCT Browsers, [Virtual.site], 1 as product_count FROM data")
product_breadth <- sqldf("SELECT P1.Browsers, P1.[Virtual.site] as a_product,
                        P2.[Virtual.site] as b_product
                        FROM product_dist_by_browser AS P1
                        INNER JOIN product_dist_by_browser AS P2 ON P1.Browsers=P2.Browsers")

product_breadth <- sqldf("WITH CTE AS (
                          SELECT Browsers,a_product,
                          1 as different_product
                          FROM product_breadth)
                         SELECT Browsers,a_product
                         ,SUM(different_product) as different_product
                         FROM CTE
                         GROUP BY Browsers,a_product")

product_breadth <- sqldf("SELECT a_product as 'Virtual.site', 
                          SUM(different_product),
                          COUNT(DISTINCT Browsers)
                          FROM product_breadth
                          GROUP BY a_product")
product_breadth <- data.frame(breakdown=product_breadth[,1],value=(product_breadth[,2]/product_breadth[,3]), stringsAsFactors = F)
product_breadth <- data.frame(kpi_filter='breadth',chart_label='product',product_breadth, stringsAsFactors = F)

products <- rbind(product_reach,product_use,product_breadth)
products <- data.frame(chart='product',
                       subchart='product',
                       hlt_filter=NA,
                       products,
                       stringsAsFactors = F)

id <- c(1:5)
factors<- c("entry","hour","region","platform","application")
columns <- c("Entry.type","Hour","Region","Platform","Application.type")
sql_columns <- c("[Entry.type]","Hour","Region","Platform","[Application.type]")

loop_df <- data.frame(id,factors,columns,sql_columns)

for(i in 1:length(loop_df$id)){
  ## Calculating reach for each product by "entry","hour","region","platform","application"
  assign(
    paste0(loop_df$factors[i],"_t_reach"),
    sqldf(
      paste0("SELECT ",loop_df$sql_columns[i],"
              ,COUNT(DISTINCT Browsers) as browser_count 
              FROM data 
              GROUP BY ",loop_df$sql_columns[i])
    )
  )
  assign(
    paste0(loop_df$factors[i],"_t_use"),
    sqldf(
      paste0("SELECT Browsers,",loop_df$sql_columns[i],"
              ,SUM(time) as time 
              FROM data
              GROUP BY Browsers,",loop_df$sql_columns[i])
    )
  )
  assign(
    paste0(loop_df$factors[i],"_t_use"),
    sqldf(
      paste0("SELECT ",loop_df$sql_columns[i],"
              ,AVG(time) as time 
              FROM ",loop_df$factors[i],"_t_use
              GROUP BY ",loop_df$sql_columns[i])
    )
  )
  assign(
    paste0(loop_df$factors[i],"_t_breadth_list"),
    sqldf(
      paste0("SELECT DISTINCT Browsers, ",loop_df$sql_columns[i]," FROM data")
    )
  )
  
  assign(
    paste0(loop_df$factors[i],"_t_breadth"),
    sqldf(
      paste0("SELECT D.",loop_df$sql_columns[i]," 
        ,SUM(P.product_count) as product_count
        ,COUNT(DISTINCT D.Browsers) as browser_dist_count 
        FROM ",loop_df$factors[i],"_t_breadth_list AS D 
        INNER JOIN product_dist_by_browser AS P ON D.Browsers=P.Browsers
        GROUP BY ",loop_df$sql_columns[i])
    )
  )
  
  
  assign(
    paste0(loop_df$factors[i],"_reach"),
    sqldf(
        paste0("SELECT D.[Virtual.site] 
              ,D.",loop_df$sql_columns[i],"
              ,COUNT(DISTINCT D.Browsers) as browser_count 
              ,R.value as total_browsers
              ,T.browser_count as overall_value
              FROM data AS D
              INNER JOIN product_reach AS R ON R.breakdown=D.[Virtual.site]
              INNER JOIN ",loop_df$factors[i],"_t_reach AS T ON T.",loop_df$sql_columns[i],"=D.",loop_df$sql_columns[i],"
              GROUP BY D.[Virtual.site], D.",loop_df$sql_columns[i],", R.value")
        )
  )
  assign(
    paste0(loop_df$factors[i],"_reach"),
    data.frame(kpi_filter='reach',
               chart_label=loop_df$factors[i],
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"_reach")))[,1],
               chart_breakdown=eval(parse(text = paste0(loop_df$factors[i],"_reach")))[,2],
               value=eval(parse(text = paste0(loop_df$factors[i],"_reach")))[,3],
               total_browsers=eval(parse(text = paste0(loop_df$factors[i],"_reach")))[,4],
               overall_value=eval(parse(text = paste0(loop_df$factors[i],"_reach")))[,5],
               stringsAsFactors = F)
  )
  
  ## Calculating use for each product by "entry","hour","region","platform","application"
  
  assign(
    paste0(loop_df$factors[i],"_use"),
    sqldf(
      paste0("SELECT Browsers 
            ,[Virtual.site] 
            ,",loop_df$sql_columns[i],"
            ,SUM(time) as time
            FROM data 
            GROUP BY Browsers, [Virtual.site], ",loop_df$sql_columns[i])
    )
  )
  assign(
    paste0(loop_df$factors[i],"_use"),
    sqldf(
      paste0("SELECT [Virtual.site] 
            ,D.",loop_df$sql_columns[i],"
            ,AVG(D.time) as time
            ,T.time as overall_value
            FROM ",loop_df$factors[i],"_use AS D
            INNER JOIN ",loop_df$factors[i],"_t_use AS T ON D.",loop_df$sql_columns[i],"=T.",loop_df$sql_columns[i],"
            GROUP BY [Virtual.site], D.",loop_df$sql_columns[i])
    )
  )
  
  assign(
    paste0(loop_df$factors[i],"_use"),
    data.frame(kpi_filter='use',
               chart_label=loop_df$factors[i],
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"_use")))[,1],
               chart_breakdown=eval(parse(text = paste0(loop_df$factors[i],"_use")))[,2],
               value=eval(parse(text = paste0(loop_df$factors[i],"_use")))[,3],
               total_browsers=NA,
               overall_value=eval(parse(text = paste0(loop_df$factors[i],"_use")))[,4],
               stringsAsFactors = F)
  )
  
  ## Calculating breadth for each product by "entry","hour","region","platform","application"
  
  assign(
    paste0(loop_df$factors[i],"_breadth"),
    sqldf(
      paste0("SELECT DISTINCT Browsers
             ,[Virtual.site]
             ,",loop_df$sql_columns[i],"
             FROM data")
    )
  )
  
  assign(
    paste0(loop_df$factors[i],"_breadth"),
    sqldf(
      paste0("SELECT P1.Browsers
            ,P1.",loop_df$sql_columns[i]," 
            ,P1.[Virtual.site] as a_product
            ,P2.[Virtual.site] as b_product
             FROM ",loop_df$factors[i],"_breadth AS P1
             INNER JOIN ",loop_df$factors[i],"_breadth  AS P2 ON P1.Browsers=P2.Browsers
             AND P1.",loop_df$sql_columns[i],"=P2.",loop_df$sql_columns[i])
      )
    )

  assign(
    paste0(loop_df$factors[i],"_breadth"),
    sqldf(
      paste0("WITH CTE AS (
             SELECT Browsers
             ,",loop_df$sql_columns[i],"
             ,a_product
             ,1 as different_product
             FROM ",loop_df$factors[i],"_breadth
             )
             SELECT Browsers
             ,",loop_df$sql_columns[i],"
             ,a_product
             ,SUM(different_product) as different_product
             FROM CTE
             GROUP BY Browsers, ",loop_df$sql_columns[i],", a_product")
      )
    )
  assign(
    paste0(loop_df$factors[i],"_breadth"),
    sqldf(
      paste0("SELECT B.a_product as 'Virtual.site'
            ,B.",loop_df$sql_columns[i],"
            ,SUM(B.different_product)
            ,COUNT(DISTINCT B.Browsers)
            ,SUM(T.product_count)/SUM(T.browser_dist_count) as overall_value
            FROM ",loop_df$factors[i],"_breadth AS B
            INNER JOIN ",loop_df$factors[i],"_t_breadth AS T ON T.",loop_df$sql_columns[i],"=B.",loop_df$sql_columns[i],"
            GROUP BY a_product, B.",loop_df$sql_columns[i])
    )
  )
  
  
  assign( 
    paste0(loop_df$factors[i],"_breadth"),
    data.frame(kpi_filter='breadth',
               chart_label=loop_df$factors[i],
               breakdown=eval(parse(text = paste0(loop_df$factors[i],"_breadth")))[,1],
               chart_breakdown=eval(parse(text = paste0(loop_df$factors[i],"_breadth")))[,2],
               value=(eval(parse(text = paste0(loop_df$factors[i],"_breadth")))[,3]/eval(parse(text = paste0(loop_df$factors[i],"_breadth")))[,4]),
               total_browsers=NA,
               overall_value=eval(parse(text = paste0(loop_df$factors[i],"_breadth")))[,5],
               stringsAsFactors = F)
  )
  
}
entry <- rbind(entry_reach,entry_use,entry_breadth)
entry <- data.frame(chart='how', subchart='entry',hlt_filter='how',
                    entry,
                    stringsAsFactors = F)

platform <- rbind(platform_reach,platform_use,platform_breadth)
platform <- data.frame(chart='how', subchart='platform',hlt_filter='how',
                       platform,
                       stringsAsFactors = F)

application <- rbind(application_reach,application_use,application_breadth)
application <- data.frame(chart='how', subchart='application',hlt_filter='how',
                          application,
                          stringsAsFactors = F)

middle_how <- rbind(entry,platform,application)

region <- rbind(region_reach,region_use,region_breadth)
region <- data.frame(chart='who', subchart='region',hlt_filter='who',
                     region,
                     stringsAsFactors = F)

hour <- rbind(hour_reach,hour_use,hour_breadth)
hour <- data.frame(chart='when', subchart='hour',hlt_filter='when',
                   hour,
                   stringsAsFactors = F)

hlt <- data.frame(
  chart='hlt',
  subchart='hlt',
  hlt_filter=c("who","how","when"),
  kpi_filter=NA,
  chart_label=c("Who","How","When"),
  breakdown=NA,
  chart_breakdown=c('A','B','C'),
  value=NA,
  total_browsers=NA,
  overall_value=NA,
  stringsAsFactors = F
)

left_charts <- rbind(kpis,products)
left_charts <- data.frame(left_charts[,1:6],chart_breakdown=NA,value=left_charts[,7], total_browsers=NA,overall_value=NA)

tableau_data <- rbind(left_charts,middle_how,region,hour,hlt)

# Loading in co-ordinates for a regional hex plot
hex_plot <- read.csv("uk_region_hex_plot.csv")

tableau_data  <- sqldf("SELECT * FROM tableau_data AS T LEFT JOIN hex_plot AS H ON T.chart_breakdown=H.Region")