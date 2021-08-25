import json

class AppData:
    def __init__(self, dbFileName):
        self.__dbFileName = dbFileName
        self.read_app_data()

    def get_data(self):
        return self.__app_data["apps"]

    def read_app_data(self):
        with open(self.__dbFileName) as f:
            self.__app_data = json.load(f)

    def write_app_data(self):
        with open(self.__dbFileName, 'w') as f:
            json.dump(self.__app_data, f)

    def increment_upvote_count(self, app_id):
        app_idx = next((idx for (idx, app) in enumerate(self.__app_data["apps"]) if app["id"] == app_id), None)

        if app_idx == None:
            raise ValueError("App id not found.")

        self.__app_data["apps"][app_idx]["upvotes"] += 1

        self.write_app_data()
