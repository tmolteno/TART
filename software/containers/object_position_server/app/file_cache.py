import datetime
import urllib.request
import os
import traceback
import logging

import sky_object
import tart.util.utc as utc


class FileCache(sky_object.SkyObject):
    def __init__(self, name):
        sky_object.SkyObject.__init__(self, name)
        self.cache_root = "./orbit_data/{}".format(self.name)
        self.last_download_attempt = {}
        self.cache = {}

    def get_url(self, utc_date):
        doy = "%.3d" % utc_date.yday()
        yy = "%.2d" % (utc_date.year()-2000)
        yyyy = utc_date.year()
        path = f"daily/{yyyy}/brdc/brdc{doy}0.{yy}n"
        return f"ftp://cddis.gsfc.nasa.gov/gps/data/{path}"

    def get_local_filename(self, utc_date):
        return "{}/{}/{}.dat".format(utc_date.year, utc_date.month, utc_date.day)

    def get_local_path(self, fname):
        return "{}/{}".format(self.cache_root, fname)

    def create_object_from_file(self, local_path):
        # Override to create the object from the file
        pass

    def download_file(self, url, local_file):
        try:
            os.makedirs(os.path.dirname(local_file))
        except Exception:
            pass
        try:
            if (url in self.last_download_attempt):
                print(f"Download Attempt: {self.last_download_attempt}")
                last_try = self.last_download_attempt[url]
                print(f"last_try: {last_try}")
                delta_seconds = (datetime.datetime.now() -
                                 last_try).total_seconds()
                if last_try and (delta_seconds < 3600):
                    raise RuntimeError(
                        f"Error ({url} -> {local_file}: Already attempted ({last_try} {delta_seconds}")

            logging.info("starting download ({} -> {}".format(url, local_file))
            self.last_download_attempt[url] = datetime.datetime.now()
            dat = urllib.request.urlopen(url)
            with open(local_file, 'wb') as w:
                w.write(dat.read())
                w.close()
            logging.info("download complete")
        except Exception as err:
            logging.exception(err)
            self.last_download_attempt[url] = datetime.datetime.now()
            raise (err)

    def get_object(self, date):
        utc_date = utc.to_utc(date)

        fname = self.get_local_filename(utc_date)
        if fname in self.cache:
            return self.cache[fname]

        try:
            local_path = self.get_local_path(fname)

            if (os.path.isfile(local_path) is False):
                self.download_file(self.get_url(utc_date), local_path)

            self.cache[fname] = self.create_object_from_file(local_path)
            return self.cache[fname]
        except Exception as error:
            # Something went horribly wrong. print(out the exception and use data from a day ago)
            tb = traceback.format_exc()
            logging.error(tb)
            logging.error("Something went wrong. Using old orbit information")
            return self.get_object(date - datetime.timedelta(days=1))
