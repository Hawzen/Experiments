class User:

    def __init__(self, user: object):
        self.json = user._json
        self.user = user
        self.cursor = (0, -1)
        self.friendsIds = set()
        self.done = False  # For checking weather the user was examined or not, used in searching for friends

    def addFriend(self, friends):
        self.friendsIds.update(friends)


class Target:

    def __init__(self, userName):
        self.userName = userName
        self.cursor = (0, -1)
        self.friends = {}
        self.doneCounter = 0

    def searchFriends(self, api, depth=5000):
        """Gets friends of target and stores them in dictionary with IDs representing keys and USER objects
        representing values If limit rate error then store what was gotten of of dict into self dictionary and store
        the cursor for future """
        output = getFriends(api=api, userName=self.userName, friendNum=depth, cursor=self.cursor)
        if type(output) != dict:
            self.friends.update(output[0])
            self.cursor = output[1]
        else:
            self.friends.update(output)

    def searchFOF(self, api):
        limit = api.rate_limit_status(resources="friends")["resources"]['friends']['/friends/list']['remaining']
        print("Limit is {}".format(limit))

        for user in self.friends.values():
            if limit < 1:
                break
            limit -= 1

            if not user.done:
                getFriendsIds(api, user)
                self.doneCounter += 1
        print("{} Left".format(len(self.friends) - self.doneCounter))


from api_funcs import getFriends, getFriendsIds
