#!/usr/bin/python

from sys import argv

d = argv[1]

print "%s/%s/%s" % (d[0:4], d[4:6], d[6:8])
