#!/usr/bin/python

from sys import argv
from datetime import date
from time import strptime, strftime, gmtime
from trackcolour import gnuplot_colour

#from upoints.gpx import Trackpoint
from upoints import point


import xml.parsers.expat

d = argv[1]
today = date(int(d[0:4]), int(d[4:6]), int(d[6:8]))

class Stats:
    def __init__(self, outfile):
        self.outfile = outfile
        self.start_time = None
        self.end_time = None
        self.total_dist = 0
        self.ascend_total = 0
        self.descend_total = 0
        self.time_walking = 0
        self.time_cycling = 0
        self.num_stops = 0
        self.avg_walking = None
        self.avg_cycling = None

    def header(self):
        self.outfile.write("# time\taltitude\tspeed\tdistance\n")

    def footer(self):
        if self.total_dist == 0:
            # gnuplot doesn't accept datafiles with no points
            # so write a zero point in the case where there was no data
            self.outfile.write("0\t0\t0\t0\n")

    def do_stats(self, cur_point, last_point):
        if last_point is not None:
            b = point.Point(cur_point.lat, cur_point.lon)
            a = point.Point(last_point.lat, last_point.lon)
            dst = b.distance(a)
            self.total_dist += dst

        self.outfile.write("%s\t%f\t%f\t%f\n" %
                           (strftime("%H:%M:%S", cur_point.time_gmt),
                            cur_point.ele,
                            cur_point.speed,
                            self.total_dist))


in_map = {}

class GpxPoint:
    def __init__(self, lat, lon):
        self.lat = lat
        self.lon = lon
        self.speed = 0
        self.ele = 0
        self.time_gmt = gmtime(0)

last_point = None
cur_point = None

def in_elem(name):
    global in_map
    if name in in_map:
        return in_map[name]
    return False

def start_element(name, attrs):
    global in_map, cur_point, last_point, cdata
    in_map[name] = True
    cdata = ""

    if name == "trkpt":
        last_point = cur_point
        cur_point = GpxPoint(float(attrs["lat"]), float(attrs["lon"]))

def gpx_time(cdata):
    # ignore milliseconds
    FMT = "%Y-%m-%dT%H:%M:%S"
    LEN = 19
    return strptime(cdata[0:LEN], FMT)

def end_element(name):
    global in_map, cur_point, last_point, stats, cdata

    if in_elem("trkpt"):
        if in_elem("ele"):
            cur_point.ele = float(cdata)
        if in_elem("time"):
            cur_point.time_gmt = gpx_time(cdata)
        if in_elem("speed"):
            cur_point.speed = float(cdata)

    in_map[name] = False

    if name == "trkpt":
        stats.do_stats(cur_point, last_point)

def char_data(data):
    global cdata
    cdata += data

p = xml.parsers.expat.ParserCreate()

p.StartElementHandler = start_element
p.EndElementHandler = end_element
p.CharacterDataHandler = char_data

stats = Stats(open("%s.dat" % d, "w"))
stats.header()

p.ParseFile(open("%s.gpx" % d))

stats.footer()
