#!/usr/bin/python

from data import data
from tracktitle import track_title

print "var tourData = new Array(%d);" % len(data)
print
for i, day in enumerate(data):
    print "tourData[%d] = ['%s', '%s'];" % (i, day[0], track_title(day[0]))
