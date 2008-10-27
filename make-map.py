#!/usr/bin/python

from data import data
from tracktitle import track_title
from subprocess import Popen, PIPE

# hmm I wish python had a builtin XSLT module
def get_bounds(day):
    xslt = Popen(["xsltproc", "../jsbounds.xsl", day + ".gpx"], stdout=PIPE)
    return xslt.stdout.read()

def quote(s):
    return "'%s'" % s[0]

print "tourDates = [ %s ];" % ", ".join(map(quote, data))
print
print "var tourData = new Array(%d);" % len(data)
print
for i, day in enumerate(data):
    print "tourData['%s'] = [%d, '%s', %s];" % (day[0], i, track_title(day[0]), get_bounds(day[0]))

