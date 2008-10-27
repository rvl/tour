#!/usr/bin/python

from data import data, pathify
from sys import stdin, stdout, stderr, argv

def print_header():
    global date
    stdout.write("""photos['%s'] = new Array();

""" % date)

def print_footer():
    stdout.write("""

""")

def write_untagged(gid, filename):
    pass

def write_tagged(gid, filename, lat, lon):
    global date

    url = "http://gallery.rodney.id.au/v/tour%s/%s" % (pathify(date), filename)
    stdout.write("photos['%s'].push(['%s', %s, %s]);\n" % (date, gid, lat, lon))

def process_line(line):
    (gid, directory, filename, lat, lon) = line.split()
    if lat == '-' or lon == '-':
        write_untagged(gid, filename)
    else:
        write_tagged(gid, filename, lat, lon)

date = argv[1]

print_header()

line = stdin.readline()

while line is not None and line != "":
    process_line(line)
    line = stdin.readline()

print_footer()
