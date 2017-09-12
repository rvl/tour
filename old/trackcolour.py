#!/usr/bin/python

from sys import argv
from datetime import date

colours = [(0xff, 0x00, 0xff, "mon"),
           (0xc0, 0x00, 0x00, "tue"),
           (0xff, 0x80, 0x00, "wed"),
           (0xff, 0xff, 0x00, "thu"),
           (0x00, 0xc0, 0x00, "fri"),
           (0x00, 0x00, 0xc0, "sat"),
           (0x80, 0x00, 0xff, "sun")]

def weekday(d):
    try:
        return date(int(d[0:4]), int(d[4:6]), int(d[6:8])).weekday()
    except ValueError:
        return 0 # maybe I'm shooting my foot here

def rgb(d):
    return colours[weekday(d)][0:3]

def hex(n):
    return "%02x" % n

def hrgb(d):
    return tuple(map(hex, rgb(d)))

def google_colour(d):
    c = hrgb(d)
    return "c0%s%s%s" % (c[2], c[1], c[0])

def gnuplot_colour(d):
    return "#%s%s%s" % hrgb(d)

def mapserver_colour(d):
    return "%d %d %d" % rgb(d)

def print_key_javascript():
    print "var colours = new Array(%d);" % len(colours)
    for i, c in enumerate(colours):
        print "colours[%d] = ['#%s%s%s', '%s'];" % (i, hex(c[0]), hex(c[1]),
                                                    hex(c[2]), c[3])

# silly silly silly silly
if __name__ == "__main__":
    f = google_colour
    d = argv[1]

    if argv[1] == '-j':
        print_key_javascript()
    else:
        if argv[1] == '-g':
            f = gnuplot_colour
            d = argv[2]
        elif argv[1] == '-k':
            d = argv[2]
        elif argv[1] == '-m':
            f = mapserver_colour
            d = argv[2]

        print f(d)
