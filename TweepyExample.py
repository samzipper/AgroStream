# Learning how to use the Twitter streaming API
# Source: http://www.youtube.com/watch?v=pUUxmvvl2FE (Part 1, how to stream tweets)
# Source: http://www.youtube.com/watch?v=d-Et9uD463A (Part 2, how to save tweets)
# Source: http://www.youtube.com/watch?v=AtqqVXZ365g (Part 3, cleaning up tweets)
#
from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener
import time
import json

# authentication data- get this info from twitter after you create your application
# read it in from a file that's not tracked on GitHub because it's secret
auth = open("TwitterAuth.txt","r")
auth_data = auth.read().splitlines()
auth.close()
ckey = auth_data[3]
csecret = auth_data[4]
atoken = auth_data[5]
asecret = auth_data[6]
print("ckey=",ckey)
print("csecret=",csecret)
print("atoken=",atoken)
print("asecret=",asecret)

# define listener class
class listener(StreamListener): 

	def on_data(self, data):
		try:
			print(data)   # this write the whole tweet to terminal
			# all data is written as JSON
			# Goal: split data up into smaller pieces, and only store the pieces you want
			# Find the text right before the tweet in JSON: ,"text":"
			# Find the text right after the tweet in JSON: 
#			tweet = data.split(',"text":"')[1].split('","source')[0]  # splitting it with a [1] to get stuff to the right of ,"text":" and a [0] to get the area to the left of ","source
#			print tweet
			
		
			
			# time.time() gives the Unix timestamp
			#saveThis = str(time.time())+'::'+tweet  # define the format you want to save; '::' to separate tweet from timestamp
			
			#saveFile = open('saveJustTweets.csv','a')  # set up CSV file to save data, append (rather than overwrite)
			#saveFile.write(saveThis)  # write the data (instead of saveThis, could have just 'data' and it will save the entire JSON string
			#saveFile.write('\n')  # put in a new line
			#saveFile.close
			return True
		except BaseException as e:
			print('failed on data, ', str(e))  # if there is an error, show what it is
			time.sleep(5)  # one error could be that you're rate-limited; this will cause the script to pause for 5 seconds
		 			
	def on_error(self, status):
		print(status)
		
# authenticate yourself
auth = OAuthHandler(ckey, csecret)
auth.set_access_token(atoken, asecret)
twitterStream = Stream(auth, listener())
twitterStream.filter(track=["corn"])  # track what you want to search for!