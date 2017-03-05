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
