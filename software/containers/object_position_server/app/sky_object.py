class SkyObject:

    def __init__(self, name):
        self.name = name

    def get_positions(self, date):
        raise Exception(
            "This cache object must have an overriden get_positions() method")

    def get_az_el(self, date, lat, lon, alt):
        raise Exception(
            "This cache object must have an overriden get_az_el() method")
