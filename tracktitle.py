#!/usr/bin/python

from sys import argv
from datetime import date
from data import data

d = argv[1]

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

stuff = find_date(argv[1])
n = date_num(argv[1]) + 1

if stuff[2] is None:
    print "Day %d: %s" % (n, stuff[1])
else:
    print "Day %d: %s - %s" % (n, stuff[1], stuff[2])
