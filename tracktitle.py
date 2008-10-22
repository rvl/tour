#!/usr/bin/python

from sys import argv
from datetime import date
from data import data

def date_num(d):
    for n, stuff in enumerate(data):
         if d == stuff[0]:
            return n
    return None

def find_date(d):
    for stuff in data:
        if d == stuff[0]:
            return stuff
    return None

def track_title(d):
    stuff = find_date(d)
    n = date_num(d) + 1

    title = "Day %d: %s" % (n, stuff[1])
    if stuff[2] is not None:
        title = "%s - %s" % (title, stuff[2])

    return title

if __name__ == "__main__":
    print track_title(argv[1])
