import json
import requests
import tweepy
from threading import Thread
import datetime
import subprocess
import sys
import time
from queue import Queue
import queue
import re
import atexit
from textblob import TextBlob


class DateEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%Y-%m-%d %H:%M:%S")
        else:
            return json.JSONEncoder.default(self, obj)


class TwitterAPIV2(Thread):
    def __init__(self, tokens, channel):
        Thread.__init__(self)
        self.tokens = tokens
        self.bearer = self.tokens[0]["bearer_token"]
        auth = tweepy.OAuthHandler(self.tokens[0]["consumer_key"], self.tokens[0]["consumer_secret"])
        auth.set_access_token(self.tokens[0]["access_token"], self.tokens[0]["access_token_secret"])
        self.api = tweepy.API(auth, wait_on_rate_limit=False)
        self.currentToken = 0
        self.channel = channel
        self.running = True

        self.user_url  = "https://api.twitter.com/2/users/{}/tweets"
        #print("Currently using twitter token: ", self.currentToken + 1, "/", len(self.tokens))

    def nextToken(self):
        self.currentToken = (self.currentToken+1) % len(self.tokens)
        self.bearer = self.tokens[self.currentToken]["bearer_token"]
        self.api.auth.access_token = self.tokens[self.currentToken]["access_token"]
        self.api.auth.access_token_secret = self.tokens[self.currentToken]["access_token_secret"]
        self.api.auth.consumer_key = str.encode(self.tokens[self.currentToken]["consumer_key"], encoding='utf-8')
        self.api.auth.consumer_secret = str.encode(self.tokens[self.currentToken]["consumer_secret"], encoding='utf-8')
        #print("Currently using twitter token: ", self.currentToken + 1, "/", len(self.tokens))

    def getHeader(self):
        return {"Authorization": "Bearer {}".format(self.bearer)}

    def getUserTimeline(self, user_id):
        url = self.user_url.format(user_id)

        # fetch first period 2020-01-01 - 2021-03-30
        ret_1 = self.fetch_user_tweet_between(url, "2020-01-01T00:00:50.52Z", "2020-06-30T23:59:59.52Z")
        # fetch second period 2020-10-01 - 2021-03-30
        ret_2 = self.fetch_user_tweet_between(url, "2020-10-01T00:00:50.52Z", "2021-03-30T23:59:59.52Z")

        return ret_1 + ret_2

    def fetch_user_tweet_between(self, url, start_time, end_time):
        ret_array = []
        pag_token = "placeholder"
        while pag_token is not None:
            repeat = True
            while repeat:
                if pag_token == "placeholder":
                    pag_token = None

                response = self.sending_request(url, start_time, end_time, pag_token)
                if response.status_code != 200:
                    print(response.status_code, " ", response.text)
                    repeat = True
                    self.nextToken()
                else:
                    repeat = False
                    response_str = response.json()
                    ret_array = ret_array + response_str["data"]
                    if response_str["meta"].get("next_token") is not None:
                        pag_token = response_str["meta"]["next_token"]
                    else:
                        pag_token = None

        # fetch second period

        return ret_array

    def sending_request(self, url,start_time, end_time, pagination_token=None):
        if pagination_token is None:
            response = requests.request("GET",
                                        url,
                                        headers=self.getHeader(),
                                        params={"start_time": start_time,
                                                "end_time": end_time,
                                                "tweet.fields": "created_at,author_id,entities,geo",
                                                "max_results":"100",
                                                # this works, keep this
                                                "expansions": "geo.place_id",
                                                # this param does not work, and not affect anything
                                                "place.fields": "contained_within,country,country_code,full_name,geo,id,name,place_type"})
                                                # this param also does not work, and not affect anything
            return response
        else:
            response = requests.request("GET",
                                        url,
                                        headers=self.getHeader(),
                                        params={"start_time": start_time,
                                                "end_time": end_time,
                                                "tweet.fields": "created_at,author_id,entities,geo",
                                                "max_results": "100",
                                                "pagination_token":pagination_token,
                                                # this works, keep this
                                                "expansions": "geo.place_id",
                                                # this param does not work, and not affect anything
                                                "place.fields": "contained_within,country,country_code,full_name,geo,id,name,place_type"})
                                                # this param also does not work, and not affect anything
            return response


    def run(self):
        while self.running:
            try:
                read = self.channel.get(block=True, timeout=1)
                self.channel.queue.clear()
                self.nextToken()
            except:
                pass

class UserFetcher(Thread):
    def __init__(self, API, UserOut, APIUpdate, config_area):
        Thread.__init__(self)
        self.user_out = UserOut
        self.api_update = APIUpdate
        self.api = API
        self.users = {}
        #self.currentDate = datetime.datetime.today()
        self.currentDate = datetime.datetime.today() - datetime.timedelta(days=1)
        self.isNextDay = False
        self.areas = config_area
        self.running = True

    def extract_user_info(self, tweets):
        for tweet in tweets:
            # if find a new user, add it to the pending user dict
            if self.users.get(tweet.user.id) is None:
                self.users[tweet.user.id] = True
                self.user_out.put((tweet.user.id,tweet.place.bounding_box.coordinates, tweet.user.location, tweet.user.name, tweet.place.place_type))
                #print("User Harvester pushed: ", tweet.user.id)
            else:
                continue

    def fetch_users_in_week(self):
        print("[UserFetcher][Info] Initializing user extraction, fetching active users for the past week...")
        for i in range(7):
            for area in self.areas:
                repeat = True
                while repeat:
                    try:
                        areaInfo = self.api.api.geo_search(query=area, granularity="city")[0]
                        tweets = self.api.api.search(
                            q="place:%s" % areaInfo.id,
                            count=200,
                            until=(self.currentDate - datetime.timedelta(days=i)).strftime('%Y-%m-%d'),
                            lang='en'
                        )
                        self.extract_user_info(tweets)
                        repeat = False
                    except Exception as e:
                        self.api_update.put(0)
                        print("[UserFetcher][Info] ", e, " Switching tokens...")
                        time.sleep(5)

    def monitor_users(self):
        self.isNextDay = not (self.currentDate.strftime('%Y-%m-%d') == datetime.datetime.today().strftime('%Y-%m-%d'))
        if self.isNextDay:
            print("[UserFetcher][Info] Running user extraction on ", self.currentDate.strftime('%Y-%m-%d'))
            for area in self.areas:
                repeat = True
                while repeat:
                    try:
                        areaInfo = self.api.api.geo_search(query=area, granularity="city")[0]
                        tweets = self.api.api.search(
                            q="place:%s" % areaInfo.id,
                            count=200,
                            until=(self.currentDate - datetime.timedelta(days=1)).strftime('%Y-%m-%d'),
                            lang='en'
                        )
                        self.extract_user_info(tweets)
                        repeat = False
                    except Exception as e:
                        self.api_update.put(0)
                        print("[UserFetcher][Info] ", e, " Switching tokens...")
        self.currentDate = datetime.datetime.today()

    def start(self):
        """
        When harvester started, go through activated user within given region is likely causing all five api
        to exceeded rate limit, this is now disabled.

        To enable:
                (1) uncomment the code below
                (2) uncomment:  "#self.currentDate = datetime.datetime.today()" in __init__(self)
                (3) comment-out: "self.currentDate = datetime.datetime.today() - datetime.timedelta(days=1)"
        """
        # self.fetch_users_in_week()
        Thread.start(self)

    def run(self):
        while self.running:
            self.monitor_users()


class TweetFetcher(Thread):
    def __init__(self, API, userIn, APIUpdate, DBOut):
        Thread.__init__(self)
        self.api = API
        self.user_in = userIn
        self.api_update = APIUpdate
        self.db_out = DBOut
        self.running = True

    def fetch_user_tweets(self, userInfo):
        user_id = userInfo[0]
        user_coord = self.count_average_coord(userInfo[1])
        user_location = userInfo[2]
        user_name = userInfo[3]
        user_location_type = userInfo[4]
        tweets = self.api.getUserTimeline(user_id)

        # TODO process tweets
        for tweet in tweets:
            p_t = self.process_tweet(tweet, user_id, user_name, user_coord, user_location, user_location_type)
            self.db_out.put(p_t)

    def process_tweet(self, twi, user_id, user_name, coord, loc, loc_type):
        res = dict()

        res['_id'] = str(user_id) + ":" + str(twi['id'])
        res['Tweet_id'] = twi['id']
        res['User_id'] = str(user_id)
        res['User_name'] = user_name
        time = twi['created_at']
        time = time.split("T")[0]
        dates = time.split("-")
        if dates[0] == "2020" and int(dates[1]) <= 3:
            res['Period'] = "2020 1-3"
        elif dates[0] == "2020" and int(dates[1]) >= 4 and int(dates[1]) <= 6:
            res['Period'] = "2020 4-6"
        elif dates[0] == "2020" and int(dates[1]) >= 10 and int(dates[1]) <= 12:
            res['Period'] = "2020 10-12"
        else:
            res['Period'] = "2021 1-3"


        res['Create_time'] = time

        if twi.get('geo') is None:
            res['User_Coordinates'] = coord
            res['User_location'] = loc
            res['User_location_Type'] = loc_type
        else:
            # get geo info by place_id
            try:
                data = self.api.api.geo_id(twi['geo']['place_id'])
                res['User_Coordinates'] = data.centroid
                res['User_Location'] = data.full_name
                res['User_Location_Type'] = data.place_type
            except Exception as e:
                self.api_update.put(0)
                res['User_Coordinates'] = coord
                res['User_location'] = loc
                res['User_location_Type'] = loc_type

        if twi.get('entities') is not None:
            if twi['entities'].get('hashtags') is not None:
                res['Hashtag'] = []
                for t in twi['entities']['hashtags']:
                    res['Hashtag'].append(t['tag'])
            else:
                res['Hashtag'] = "null"
        else:
            res['Hashtag'] = "null"


        res['Text'] = self.clean_tweet(twi['text'])
        polarity, subjectivity, indicator = self.count_sentiment(twi['text'])
        res['Polarity'] = float(format(polarity, '.3f'))
        res['Subjectivity'] = format(subjectivity, '.3f')
        res['Sentiment Indicator'] = indicator
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
        ind = 0
        if polarity < 0:
            ind = -1
        elif polarity > 0:
            ind = 1
        return polarity, subjectivity, ind

    def count_average_coord(self, coords):
        count = 0
        c_x = 0
        c_y = 0
        for c in coords[0]:
            count += 1
            c_x += float(c[0])
            c_y += float(c[1])
        c_x = c_x/count
        c_y = c_y/count
        return [c_x, c_y]

    def run(self):
        while self.running:
            try:
                userInfo = self.user_in.get(block=True, timeout=1)
                self.fetch_user_tweets(userInfo)
            except:
                pass


class CouchDBClient(Thread):
    def __init__(self, url, db_in):
        Thread.__init__(self)
        self.db_in = db_in
        self.url = url
        self.header = {"Content-Type": "application/json"}
        self.postCount = 0
        self.running = True

    def post_to_db(self, data):
        data = json.dumps(data, cls=DateEncoder)
        r = requests.post(url=self.url, headers=self.header, data=data)
        pass

    def run(self):
        while self.running:
            try:
                data = self.db_in.get(block=True, timeout=1)
                self.post_to_db(data)
                self.postCount += 1
                if self.postCount % 500 == 0:
                    print("pushed total ", self.postCount, " docs")
            except:
                pass


def install():
    subprocess.check_call([sys.executable, "-m", "pip", "install", "--user", "-r", "requirements.txt"])

def close(t1,t2,t3,t4):
    t1.running = False
    t2.running = False
    t3.running = False
    t4.running = False
    for i in range(5,0,-1):
        print("harvester shutdown in %i secs"%i)
        time.sleep(1)

def main(argv):
    user_channel, api_channel, db_channel = Queue(), Queue(), Queue()

    config = json.load(open("Config.json", 'r', encoding='utf-8'))

    api = TwitterAPIV2(config["TwitterAuth"], api_channel)
    user_fetcher = UserFetcher(api, user_channel, api_channel, config["Areas"])
    tweet_fetcher = TweetFetcher(api, user_channel, api_channel, db_channel)
    db_client = CouchDBClient(argv, db_channel)

    atexit.register(close,api,user_fetcher,tweet_fetcher,db_client)

    api.start()
    user_fetcher.start()
    tweet_fetcher.start()
    db_client.start()

if __name__ == "__main__":
    install()
    main(sys.argv[-1])
