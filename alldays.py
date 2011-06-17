#!/usr/bin/python

from data import data

import os.path

def exists(day):
    #return True
    return os.path.exists(os.path.expanduser("~/GPS_DATA/%s.gpx" % day))

print " ".join([day[0] for day in data if exists(day[0])])
