import tweepy
from classes import *
from api_funcs import *

with open('twitterKeys.txt', 'r') as file:
    lines = file.read().split('\n')
apiKey = lines[0]
apiSecretKey = lines[1]
accessToken = lines[2]
accessTokenSecret = lines[3]

auth = tweepy.OAuthHandler(apiKey, apiSecretKey)
auth.set_access_token(accessToken, accessTokenSecret)
api = tweepy.API(auth)


#Target("")

"""
    TODO: check if 'Target' friend searching works correctly
    TODO: implement the 'User' friend id searching
"""