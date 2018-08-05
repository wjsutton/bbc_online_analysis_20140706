## BBC Online Analysis
# Author: Will Sutton
# Date: 5th August 2018

R scripts used to process and extract charts for a Tableau visualisation 
Data set as of Sunday 6th July 2014

Order of scripts:
1. "process.R" used to enhance the original data provided by fixing data issues, calculating times, sessions, entry & exit points.
2. "create_charts.R" takes the enhanced dataset and produces the majority of charts for the Tableau visualisation. 
3. "create_explore_chart.R" takes the enhanced dataset and produces a "select what you like" scatter plot.

Additional files:
1. "uk_region_hex_plot.csv" takes a list of regions and assigns them an X and Y co-ordinate for a Tableau hex plot.
