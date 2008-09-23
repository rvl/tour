#!/usr/bin/python

from sys import argv
from datetime import date

colours = [("ff", "00", "ff"), # mon
           ("c0", "00", "00"), # tue
           ("ff", "80", "00"), # wed
           ("ff", "ff", "00"), # thu
           ("00", "c0", "00"), # fri
           ("00", "00", "c0"), # sat
           ("80", "00", "ff")] # sun

def weekday(d):
    return date(int(d[0:4]), int(d[4:6]), int(d[6:8])).weekday()

def google_colour(d):
    c = colours[weekday(d)]
    return "c0%s%s%s" % (c[2], c[1], c[0])

def gnuplot_colour(d):
    return "#%s%s%s" % colours[weekday(d)]

# silly silly silly silly
if __name__ == "__main__":
    f = google_colour
    d = argv[1]

    if argv[1] == '-g':
        f = gnuplot_colour
        d = argv[2]
    elif argv[1] == '-k':
        d = argv[2]

    print f(d)
