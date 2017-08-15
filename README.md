# AgroStream
AgroStream is intended to aggregate and store tweets from
the Twitter Streaming API that match keywords related to 
planting of corn/soy/wheat. The goal is to track planting 
date and compare to USDA NASS crop progress reports.

The primary work is done by the script, SearchAndStoreTweets.R.
Windows Task Scheduler is used to run this script daily, as described here:
http://stackoverflow.com/questions/2793389/scheduling-r-script
and here (to have task run in background):
http://stackoverflow.com/questions/6568736/how-do-i-set-a-windows-scheduled-task-to-run-in-the-background

***History of search strings:***
-Prior to 8/15/2017: 
    search.str <- "((corn OR soy OR wheat) AND (plant OR planting OR planted OR plants OR #plant17 OR #plant2017)) OR #corn17 OR #corn2017 OR #soy17 OR #soy2017 OR #wheat17 OR #wheat2017"
-After 8/15/2017 (did retrospective search for 8/8/2017-8/14/2017): 
    search.str.1 <- "((corn OR soy OR wheat) AND (plant OR planting OR planted OR plants OR #plant17 OR #plant2017 OR #plant18 OR #plant2018 OR harvest OR harvesting OR harvested OR harvests OR #harvest17 OR #harvest2017 OR #harvest18 OR #harvest2018))"
    search.str.2 <- "#corn17 OR #corn2017 OR #corn18 OR #corn2018 OR #corn19 OR #corn2019 OR #soy17 OR #soy2017 OR #soy18 OR #soy2018 OR #soy19 OR #soy2019 OR #wheat17 OR #wheat2017 OR #wheat18 OR #wheat2018 OR #wheat19 OR #wheat2019"
