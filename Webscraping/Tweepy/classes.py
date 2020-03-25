from api_funcs import getFriends


class User:

    def __init__(self, user: object):
        self.json = user._json
        self.user = user
        self.cursor = (0, -1)
        self.friendsIds = set()

    def addFriend(self, friends):
        map(self.friendsIds.add, friends)


class Target:

    def __init__(self, userName):
        self.userName = userName
        self.cursor = (0, -1)
        self.friends = {}

    def searchFriends(self, api, depth=5000):
        """Gets friends of target and stores them in dictionary with IDs representing keys and USER objects
        representing values If limit rate error then store what was gotten of of dict into self dictionary and store
        the cursor for future """
        output = getFriends(api=api, userName=self.userName, friendNum=depth, cursor=self.cursor)
        if len(output) == 2:
            self.friends.update(output[0])
            self.cursor = output[1]
        else:
            self.friends.update(output)