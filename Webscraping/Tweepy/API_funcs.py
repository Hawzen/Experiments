from classes import *


def checkRate(rate):
    """Returns a boolean of False when the rate function gives off values less than 1
    When a value of 1 is reached, twitter doesnt want you requesting anymore"""
    if rate() < 1:
        return False
    return True


def finished(num, cursor, dictionary):
    """Returns a boolean that signifies weather it reached the number of profiles covered or not
    num represents the number of profiles we want to cover (0 means all)"""
    # When navigating all pages
    if num == 0:
        if cursor[1] == 0:  # Reached last page
            return True
        return False
    # When navigating for set number of elements (e.g. followers)
    if len(dictionary.keys()) < num:
        return False
    return True


def getFriends(api, userName, friendNum=0, cursor=(0, -1)):
    """Returns a dict with key IDs to USER objects which store the input users' friends,
    In case it stopped due to limit rate it'll also return the dictionary and a cursor

    Input:
        api
        userName
        friendNum: the number of friends (depth) the function should return starting from the cursor, 0 means 'all' friends (default is 0)
        cursor: a cursor that keeps track where in the twitter 'friends' page we were in (default is start page)
        User: a USER object that we want to find whose friends are

    Output:
        Returns a dictionary of {ID : User object} when the number of friends specified is reached
        Returns a dictionary of {ID : User object} of users fetched so far, also returns a cursor of the last page reached
    """
    # user = self.api.get_user(user) TODO: what does this do??

    friendsDict = {}  # A dict of IDs to USER objects related which stores friends of user
    rateLimit = api.rate_limit_status(resources="friends")["resources"]["friends"]['/friends/list'][
        "remaining"]  # Gets rate for friends/list

    for _ in range(rateLimit):
        friends, cursor = api.friends(screen_name=userName, count=200, cursor=cursor[1])
        friendsDict.update({friend._json['id']: User(friend) for friend in friends})
        if finished(num=friendNum, cursor=cursor, dictionary=friendsDict):
            return friendsDict
    print("Limit Rate Reached.")
    return friendsDict, cursor


def getFriendsIds(api, user):
    """
    Input:
        api
        User: a USER object that we want to find whose friends are

    Output:
        Returns a dictionary of known ids, cursor and other stuff

    Note:
        This could be configured using cursors like getFriends but it isn't needed since one call of api.friends_ids returns
        5000 ids at once, so it has a big enough maximum
    """
    return api.friends_ids(user.json['id'], user.json['screen_name'])  # Warning: Maximum of 5000
