import tweepy
import shelve
from users import *
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

#Keep track of object target
with shelve.open("target_shelve") as sh:
    target = sh['target']

#target = Target("MohndAlrasheed")
#target.searchFriends(api)

#target.searchFOF(api)



with shelve.open("target_shelve") as sh:
    sh['target'] = target