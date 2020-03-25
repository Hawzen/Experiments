import tweepy
import pandas
import matplotlib.pyplot as plt

with open('twitterKeys.txt', 'r') as file:
    lines = file.read().split('\n')
apiKey = lines[0]
apiSecretKey = lines[1]
accessToken = lines[2]
accessTokenSecret = lines[3]
auth = tweepy.OAuthHandler(apiKey, apiSecretKey)
auth.set_access_token(accessToken, accessTokenSecret)
api = tweepy.API(auth)


class TwitterApiMethods():

    def checkRate(rate):
        if rate() < 1:
            return False
        return True

    def finished(this, num, cursor, dictionary):
        """Returns a boolean that signfies weather we reached that element we want or not
        num represents the number of elements we want to cover (0 means all)"""
        # When navigating all pages
        if num == 0:
            if cursor[1] == 0:  # Reached last page
                return True
            return False
        # When navigating for set number of elements (e.g. followers)
        if (len(dictionary.keys()) < num):
            return False
        return True


class Getter(TwitterApiMethods):

    def __init__(self, api):
        self.api = api

    def getFriends(self, userName, friendNum=0, cursor=(0, -1)):
        # Returns a dict with key IDs to USER objects which store the input users' friends,
        # In case it stopped due to limit rate it'll also return the dictionary and a curosr

        # user = self.api.get_user(user) ?? what does this do

        friendsDict = {}  # A dict of IDs to USER objects related which stores friends of user
        rateLimit = self.api.rate_limit_status(resources="friends")["resources"]["friends"]['/friends/list'][
            "remaining"]  # Gets rate for friends/list

        for _ in range(rateLimit):
            friends, cursor = api.friends(screen_name=userName, count=200, cursor=cursor[1])
            friendsDict.update({friend._json['id']: User(friend) for friend in friends})
            if self.finished(num=friendNum, cursor=cursor, dictionary=friendsDict):
                return friendsDict
        print("Limit Rate Reached.")
        return friendsDict, cursor

    def getFOFIds(self, user):
        """
        Input:
            User: a USER object that we want to find whoose friends are
            IdsScreenNames : a tuple of Ids and screen names that we want to test its relationship to base USER
            
        Output:
            Returns a tuple of known ids
        """
        self.api.friends_ids(user.json['id'], user.json['screen_name'])


class Target:

    def __init__(self, userName, api):
        self.userName = userName
        self.dictionary = {}  # dictionary with IDs represnting keys and USER objects represnting values
        self.FOFlistIds = self.dictionary.keys()  # Friends of friends list of ids
        self.getter = Getter(api)
        self.cursor = (0, -1)

    def searchFriends(self, depth=5000):
        """Gets friends of target and stores them in dictionary with IDs represnvcting keys and USER objects represnting values
        If limit rate error then store what was gotten of of dict into self dictionary and store the curosr for future"""
        output = self.getter.getFriends(userName=self.userName, friendNum=depth, cursor=self.cursor)
        if (len(output) == 2):
            self.dictionary.update(output[0])
            self.FOFlistIds = list(self.dictionary.keys())
            self.cursor = output[1]
            return
        self.dictionary.update(output)
        self.FOFlistIds = self.dictionary.keys()

    def FOFAdder(self):
        """Goes through the dictionary of IDs : USERs and adds each USER's friends to their friendsIds list, this is useful
        when used later to determine the intersections between friends of target and friends of friends of target
        e.g.: target follows A and B, A follows B -> the intersection between friends of target and friends of A is B. 
        this is to be used in visualizing a map"""
        rateLimit = api.rate_limit_status(resources="friends")["resources"]["friends"]['/friends/ids']["remaining"]
        temp = rateLimit - len(self.FOFlistIds)
        if temp < 0:
            self.FOFcursor = temp
        for friendNum in range(temp):
            user = self.dictionary[self.FOFlistIds[friendNum]]
            user.addFriend(self.getter.getFOFIds(user))  # Warning : Max of 5000 friends


class User:

    def __init__(self, user):
        self.json = user._json
        self.user = user
        self.friendsIds = []

    def addFriend(self, friendIds):
        map(self.friends.append, friendIds)


target = Target("Twitter", api)

target.searchFriends()

res = api.rate_limit_status(resources="friends")

var = res["resources"]["friends"]['/friends/ids']["remaining"]
