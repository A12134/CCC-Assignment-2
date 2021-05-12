import re
from textblob import TextBlob
import datetime
import json
import tweepy
import numpy as np
import requests
import subprocess
import sys
from threading import Thread
import time
from queue import Queue


class TwitterAPI(Thread):
    def __init__(self, tokens, request):
        Thread.__init__(self)
        self.tokens = tokens
        self.tokenIndex = 0
        self.api = None
        self.request = request

    def generate_api(self):
        token = self.tokens[self.tokenIndex]
        auth = tweepy.OAuthHandler(token["consumer_key"], token["consumer_secret"])
        auth.set_access_token(token["access_token"], token["access_token_secret"])

        api = tweepy.API(auth, wait_on_rate_limit=False)
        self.api = api
        print("Currently using twitter token: ", self.tokenIndex+1, "/", len(self.tokens))
        return api

    def generate_new_api(self):
        self.tokenIndex = (self.tokenIndex + 1) % len(self.tokens)
        return self.generate_api()

    def run(self):
        while True:
            self.generate_new_api()
            try:
                obj = self.request.get(block=True, timeout=840000)
            except:
                pass

class UserHarvester(Thread):
    def __init__(self, api, userChannel, apiChannel):
        Thread.__init__(self)
        self.api = api
        self.UserIDs = {}
        self.currentDate = datetime.datetime.today()
        self.countryID = self.api.geo_search(query="Australia", granularity="country")[0]
        self.isNextDay = True
        self.initSearch = True
        self.userChannel = userChannel
        self.apiChannel = apiChannel

    def harvest_user(self):
        """
        Gather as many user as possible within a given time frame
        limited by API only 7 days from now are available
        """
        # monitoring current date
        self.isNextDay = self.currentDate.strftime('%Y-%m-%d') == datetime.datetime.today().strftime('%Y-%m-%d')
        if self.isNextDay:
            # if the harvester is running for the first time, search users within 7 days period
            if self.initSearch:
                for i in range(7):
                    errorCount = 1
                    while errorCount != 0:
                        try:
                            tweets = self.api.search(
                                q="place:%s"%self.countryID.id,
                                count=100,
                                until=(self.currentDate - datetime.timedelta(days=i)).strftime('%Y-%m-%d'),
                                lang='en'
                            )
                            errorCount = 0
                            self.__harvest_user__(tweets)
                        except Exception as e:
                            i -= 1
                            if errorCount > 1:
                                print("Auto fix not working, check error below:")
                                print(e)
                                break
                            else:
                                self.apiChannel.put(0)
                                print(
                                    "Something went wrong, user harvester paused,trying auto fix by switching twitter api, resume in 1 second.\n")
                                time.sleep(1)
                                errorCount += 1


                self.initSearch = False
            else:
                errorCount = 1
                while errorCount!=0:
                    try:
                        tweets = self.api.search(
                            q="place:%s" % self.countryID.id,
                            count=100,
                            until=str(self.currentDate),
                            lang='en'
                        )
                        errorCount = 0
                        self.__harvest_user__(tweets)
                    except Exception as e:
                        if errorCount > 1:
                            print("Auto fix not working, check error below:")
                            print(e)
                            print('\n')
                            break
                        else:
                            self.apiChannel.put(0)
                            print(
                                "\nSomething went wrong, user harvester paused, trying auto fix by switching twitter api, resume in 5 second.")
                            time.sleep(5)
                            errorCount += 1

            self.currentDate = datetime.datetime.today()
            self.isNextDay = False

    def __harvest_user__(self, tweets):
        for tweet in tweets:
            # if find a new user, add it to the pending user dict
            if self.UserIDs.get(tweet.user.id) is None:
                self.UserIDs[tweet.user.id] = True
                self.userChannel.put((tweet.user.id, tweet.place.bounding_box.coordinates))
                #print("User Harvester pushed: ", tweet.user.id)
            else:
                continue

    def run(self):
        while True:
            self.harvest_user()


class TweetHarvester(Thread):
    def __init__(self, api, userChannel, dbChannel, apiChannel, timeLine):
        Thread.__init__(self)
        self.api = api
        self.userChannel = userChannel
        self.timeLine = timeLine
        self.dbChannel = dbChannel
        self.apiChannel = apiChannel

    def __harvesting_tweets__(self, userInfo):
        userID = userInfo[0]
        userCoord = np.mean(userInfo[1])
        errorCount = 1

        while errorCount != 0:
            try:
                for tweet in tweepy.Cursor(self.api.user_timeline, user_id=userID).items():
                    errorCount = 0
                    processed_twi = self.process(tweet, userCoord)
                    if processed_twi is None:
                        continue
                    else:
                        self.dbChannel.put(processed_twi)
                        #print("Tweet harvest pushed: ", userID)
            except Exception as e:
                if errorCount > 1:
                    print("auto fix does not work, full exception log shown below:\n")
                    print(e)
                    print('\n')
                    break
                else:
                    self.apiChannel.put(0)
                    print("\nSomething went wrong, tweet harvester paused,trying auto fix by switching twitter api, resume in 5 second.")
                    time.sleep(5)





    def run(self):
        while True:
            """
             userInfo: (userID, (lat,long))
            """
            userInfo = self.userChannel.get()
            #print("Tweet Harvester pulled: ", userInfo[0])
            self.__harvesting_tweets__(userInfo)

    def process(self, twi, coord):
        res = dict()
        year = twi.created_at.year
        month = twi.created_at.month

        if self.timeLine.get(year) is not None:
            if self.timeLine[year].get(int(month)) is not None:
                pass
        else:
            return None

        res['_id'] = str(twi.user.id) + ":" + str(twi.id)
        res['Tweet_id'] = twi.id
        res['User_id'] = twi.user.id
        res['User_name'] = twi.user.name
        res['User_location'] = twi.user.location
        res['Create_time'] = twi.created_at

        if twi.geo is None:
            res['Coordinates'] = coord
        else:
            res['Coordinates'] = twi.geo.coordinates

        if twi.entities['hashtags']:
            res['Hashtag'] = [item["text"] for item in twi.entities['hashtags']]
        else:
            res['Hashtag'] = "null"

        res['Text'] = self.clean_tweet(twi.text)
        polarity, subjectivity = self.count_sentiment(twi.text)
        res['Polarity'] = format(polarity, '.3f')
        res['Subjectivity'] = format(subjectivity, '.3f')
        return res

    def clean_tweet(self, tweet):
        """
        Utility function to clean the text in a tweet by removing
        links and special characters using regex.
        :param tweet:
        :return: Pre-processed tweet
        """
        return ' '.join(re.sub("(RT)|(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)", " ", tweet).split())

    def count_sentiment(self,text):
        blob = TextBlob(text)
        [polarity, subjectivity] = blob.sentiment
        return polarity, subjectivity

class CouchDBClient(Thread):
    def __init__(self, dataChannel, url, header):
        Thread.__init__(self)
        self.url = url
        self.header = header
        self.channel = dataChannel

    def run(self):
        while True:
            data = self.channel.get()
            print("Operation Sending to CouchDB")
            data = json.dumps(data, cls=DateEncoder)
            requests.post(url=self.url, headers=self.header, data=data)

class DateEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%Y-%m-%d %H:%M:%S")
        else:
            return json.JSONEncoder.default(self, obj)


def install():
    subprocess.check_call([sys.executable, "-m", "pip", "install", "--user", "-r", "requirements.txt"])

def main(argv):
    config = json.load(open("Config.json"))
    userChan, dbChan, apiRequestChan = Queue(), Queue(), Queue()
    apiLib = TwitterAPI(config["TwitterAuth"], apiRequestChan)
    api = apiLib.generate_api()
    userH = UserHarvester(api, userChan, apiRequestChan)
    tweetH = TweetHarvester(api, userChan, dbChan, apiRequestChan, config["Timeline"])
    db = CouchDBClient(dbChan, argv, {"Content-Type": "application/json"})
    apiLib.start()
    db.start()
    userH.start()
    tweetH.start()
    print("Harvester is running smoothly.....")

if __name__ == "__main__":
    install()
    main(sys.argv[-1])

