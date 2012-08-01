#!/usr/bin/python3
# -*- coding: utf-8 -*-

import datetime
import os.path
import sys
import csv

class TourDay(object):
    def __init__(self, row, prev_location=None):
        self.from_row(row, prev_location)

    def from_row(self, row, prev_location=None):
        self.date = self.parse_date(row[0])
        self.start = prev_location
        self.finish = row[1] or prev_location
        self.start_time = self.parse_time(row[2])
        self.finish_time = self.parse_time(row[3])
        self.dist = float(row[4]) if row[4] else 0.0
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

    @classmethod
    def csv_header(cls, writer):
        writer.writerow(["Date", "Place", "Start Time", "Finish Time",
                         "Dist", "Accom", "Transport", "Number"])

    def csv_row(self, writer):
        writer.writerow([self.strdate(), self.finish,
                         self.start_time.strftime("%H:%M") if self.start_time else None,
                         self.finish_time.strftime("%H:%M") if self.finish_time else None,
                         self.dist, self.accom, self.transport, self.number])

def read_table(table):
    days = []
    last_location = None
    for row in table:
        day = TourDay(row, last_location)
        days.append(day)
        last_location = day.finish
    return days

#data = read_table(table)

def read_csv(filename):
    days = []
    last_location = None
    reader = csv.reader(open(filename))
    next(reader)
    for row in reader:
        day = TourDay(row, last_location)
        days.append(day)
        last_location = day.finish
    return days

data = read_csv(os.path.join(os.path.dirname(__file__), "tour2012.csv"))

def pathify(date):
    return "%s/%s/%s" % chop(date)

def chop(date):
    return (date[0:4], date[4:6], date[6:8])

def get_dates():
    return [day.strdate() for day in data]

def print_dates():
    for date in get_dates():
        print(date)

def print_csv():
    writer = csv.writer(sys.stdout)
    TourDay.csv_header(writer)
    for day in data:
        day.csv_row(writer)

if __name__ == '__main__':
    print_dates()
