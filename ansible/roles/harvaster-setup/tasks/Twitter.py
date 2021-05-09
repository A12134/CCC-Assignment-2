import re
from textblob import TextBlob
import datetime
import json
import tweepy
import requests


class DateEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime("%Y-%m-%d %H:%M:%S")
        else:
            return json.JSONEncoder.default(self, obj)


def twitter_setup():
    """
    Utility function to setup the Twitter's API with our access keys provided.
    """
    consumer_key = 'K9TONA3DHJSD8LNkIBHLXGRZH'
    consumer_secret = 'CAUNPDOzqX4BLcHQijNztyZG1jaWCm0UCH0R90gBMNYRKHqoRF'
    access_token = '1384421870506713090-uXkvcSe3kjr2xlNAueY8WDU93fK3hE'
    access_token_secret = '0L6eSYLhPfuPPlu28rAhQsaxLTSOsR4w9H9EK732MSb9x'

    # Authorization and Authentication
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)

    api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)
    return api


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


def main():
    url = 'http://admin:admin@172.26.131.4:5984/twitter'  # couchDB url
    header = {"Content-Type": "application/json"}
    api = twitter_setup()
    regions = ["sydney", "melbourne", "canberra", "brisbane", "adelaide", "perth"]
    for city_name in regions:
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


if __name__ == "__main__":
    main()
