#!/usr/bin/python

from sys import argv
from datetime import date
from data import data

def date_num(d):
    for stuff in data:
         if d == stuff[0]:
             return stuff[8]
    return None

def find_date(d):
    for stuff in data:
        if d == stuff.strdate():
            return stuff
    return None

def track_title(d):
    stuff = find_date(d)
    n = stuff.number

    title = "Day %d: %s" % (n, stuff.start)
    if stuff.start != stuff.finish:
        title = "%s - %s" % (title, stuff.finish)

    return title

if __name__ == "__main__":
    print track_title(argv[1])
