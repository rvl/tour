#!/usr/bin/python
# -*- coding: utf-8 -*-

AC = "camping"
AW = "wildcamping"
AJ = "hostel"
AH = "hotel"
AP = "pension"
AS = "warmshowers"

T0 = ""
TC = "cycle"
TT = "train"
TF = "ferry"
TP = "plane"

table = [ ("20120701", "Leuven", None, None, .0, "home", T0, -1),
          ("20120702", "Köln", None, None, .0, AJ, TT, 0),
          ("20120703", "Salzburg", None, None, .0, AC, TT, 0),
          ("20120704", None, None, None, .0, AC, T0, 0),
          ("20120705", "Wien", None, None, .0, AJ, TT, 0),
          ("20120706", "Bratislava", "08:45", "15:45", 85.0, AC, TC, 1),
          ("20120707", "Ásványráró", "09:00", "18:00", 88.0, AC, TC, 2),
          ("20120708", "Komárom", "08:45", "17:45", 80.0, AC, TC, 3),
          ("20120709", "Budapest", "09:00", "19:30", 141.0, AJ, TC, 4),
          ("20120710", None, None, None, .0, AJ, T0, 0),
          ("20120711", None, None, None, .0, AJ, T0, 0),
          ("20120712", "Velence", "10:15", "20:30", 100.0, AJ, TC, 5),
          ("20120713", "Balatonkenese", "09:30", "17:00", 82.0, AP, TC, 6),
          ("20120714", "Zanka", "10:30", "16:00", 55.0, AC, TC, 7),
          ("20120715", "Keszthely", "10:00", "15:30", 58.0, AC, TC, 8),
          ("20120716", "Galambok", "10:30", "16:30", 52.0, AC, TC, 9),
          ("20120717", None, None, None, .0, AC, T0, 0),
          ("20120718", "Varaždin", "00:00", "00:00", 0.0, AH, TC, 10),
          ("20120719", "Zagreb", "00:00", "00:00", 109.0, AH, TC, 11),
          ("20120720", None, None, None, 0.0, AH, T0, 0),
      ]

import datetime
import os.path

class TourDay(object):
    def __init__(self, row, prev_location=None):
        self.date = self.parse_date(row[0])
        self.start = prev_location
        self.finish = row[0] or prev_location
        self.start_time = self.parse_time(row[2])
        self.finish_time = self.parse_time(row[3])
        self.dist = float(row[4])
        self.accom = row[5]
        self.transport = row[6]
        self.number = row[7]

    @staticmethod
    def parse_date(text):
        return datetime.datetime.strptime(text, "%Y%m%d").date()

    @staticmethod
    def parse_time(text):
        if text:
            return datetime.datetime.strptime(text, "%H:%M").time()
        return None

    def strdate(self):
        return self.date.strftime("%Y%m%d")

    def get_dict(self):
        return {
            "date": self.strdate(),
            "start": self.start,
            "finish": self.finish,
            "start_time": self.start_time.strftime("%H:%M") if self.start_time else None,
            "finish_time": self.finish_time.strftime("%H:%M") if self.finish_time else None,
            "dist": self.dist,
            "accom": self.accom,
            "transport": self.transport,
            "number": self.number,
        }

    def gpx_path(self):
        return os.path.expanduser("~/GPS_DATA/%s.gpx" % self.strdate())

    def gpx_exists(self):
        return os.path.exists(self.gpx_path())

def read_table(table):
    days = []
    last_location = None
    for row in table:
        day = TourDay(row, last_location)
        days.append(day)
        last_location = day.finish
    return days

data = read_table(table)

def pathify(date):
    return "%s/%s/%s" % chop(date)

def chop(date):
    return (date[0:4], date[4:6], date[6:8])

def get_dates():
    return [day.strdate() for day in data]

def print_dates():
    for date in get_dates():
        print(date)

if __name__ == '__main__':
    print_dates()
