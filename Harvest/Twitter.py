#1
import re
from textblob import TextBlob
import datetime
import json
import tweepy
import requests
import subprocess
import sys

class DateEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%Y-%m-%d %H:%M:%S")
        else:
            return json.JSONEncoder.default(self, obj)

class TwitterAPI:
    def __init__(self):
        f = open("TwitterAuth.json", 'r', encoding='utf-8')
        self.tokens = json.load(f)
        f.close()
        self.tokenIndex = 0

    def generate_api(self):
        token = self.tokens[self.tokenIndex]
        auth = tweepy.OAuthHandler(token["consumer_key"], token["consumer_secret"])
        auth.set_access_token(token["access_token"], token["access_token_secret"])

        api = tweepy.API(auth, wait_on_rate_limit=False)
        return api

    def generate_new_api(self):
        self.tokenIndex = (self.tokenIndex + 1) % len(self.tokens)
        return self.generate_api()


def is_alphabet(uchar):
    if (u'\u0041' <= uchar <= u'\u005a') or (u'\u0061' <= uchar <= u'\u007a'):
        return True
    return False


def clean_tweet(tweet):
    """
    Utility function to clean the text in a tweet by removing
    links and special characters using regex.
    :param tweet:
    :return: Pre-processed tweet
    """
    return ' '.join(re.sub("(RT)|(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)", " ", tweet).split())


def process(twi):
    res = dict()
    search = False
    year = twi.created_at.year
    month = twi.created_at.month
    unexpected_month = [7, 8, 9]
    if year == 2021 and month < 4:
        search = True
    elif year == 2020 and month not in unexpected_month:
        search = True
    elif year == 2019:
        return -1
    if search:
        res['_id'] = str(twi.user.id) + ":" + str(twi.id)
        res['Tweet_id'] = twi.id
        res['User_id'] = twi.user.id
        res['User_name'] = twi.user.name
        res['User_location'] = twi.user.location
        res['Creat_time'] = twi.created_at
        if twi.entities['hashtags']:
            res['Hashtag'] = [item["text"] for item in twi.entities['hashtags']]
        else:
            res['Hashtag'] = "null"
        res['Text'] = clean_tweet(twi.text)
        polarity, subjectivity = count_sentiment(twi.text)
        res['Polarity'] = format(polarity, '.3f')
        res['Subjectivity'] = format(subjectivity, '.3f')
    return res


def count_sentiment(text):
    blob = TextBlob(text)
    [polarity, subjectivity] = blob.sentiment
    return polarity, subjectivity

def harvest_to_couchdb(api,url,header,city_name):
    try:
        places = api.geo_search(query=city_name, granularity="city")
        place_id = places[0].id
        # i = MAX_QUERIES
        tweets = api.search(q="place:%s" % place_id)
        text = tweets[0].text
        while not is_alphabet(text[1]):
            tweets = api.search(q="place:%s" % place_id)
            text = tweets[0].text
        text_user_id = tweets[0].user.id
        for tweet in tweepy.Cursor(api.user_timeline, user_id=text_user_id).items():
            res = process(tweet)
            if res == -1:
                break
            elif res:
                res = json.dumps(res, cls=DateEncoder)
                requests.post(url=url, headers=header, data=res)

        return True

    except Exception:
        return False

def main(argv):
    url = argv   # couchDB url
    header = {"Content-Type": "application/json"}
    apiData = TwitterAPI()
    api = apiData.generate_api()
    regions = ["sydney", "melbourne", "canberra", "brisbane", "adelaide", "perth"]
    for city_name in regions:
        while not harvest_to_couchdb(api, url, header, city_name):
            api = apiData.generate_new_api()

def install():
    subprocess.check_call([sys.executable, "-m", "pip", "install", "--user", "-r", "requirements.txt"])

if __name__ == "__main__":
    install()
    main(sys.argv[-1])
