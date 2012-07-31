#!/usr/bin/python

from sys import argv
from datetime import date, datetime
from time import strptime, strftime, gmtime
from trackcolour import gnuplot_colour

#from upoints.gpx import Trackpoint
from upoints import point

import pylab as pl
import numpy as np
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap, BoundaryNorm


import xml.parsers.expat

class DatWriter:
    def __init__(self, outfile):
        self.outfile = outfile
        self.total_dist = 0

    def header(self):
        self.outfile.write("# time\taltitude\tspeed\tdistance\n")

    def footer(self):
        if self.total_dist == 0:
            # gnuplot doesn't accept datafiles with no points
            # so write a zero point in the case where there was no data
            self.outfile.write("0\t0\t0\t0\n")

    def write_point(self, cur_point, total_dist):
        self.total_dist = total_dist
        self.outfile.write("%s\t%f\t%f\t%f\n" %
                           (strftime("%H:%M:%S", cur_point.time_gmt),
                            cur_point.ele, cur_point.speed,
                            total_dist))

class Matplotter:
    def __init__(self):
        self.points = []

    def write_point(self, cur_point, total_dist):
        dt = gmtime2datetime(cur_point.time_gmt)
        num = mdates.date2num(dt)
        self.points.append([num, cur_point.ele,
                            cur_point.speed, total_dist])

    def go(self):
        ar = np.array(self.points, float)

        time = ar[:,0]
        ele = ar[:,1]
        spd = ar[:,2]
        dist = ar[:,3]

        fig = plt.figure()

        #self.speed_time(fig, time, spd)
        self.profile(fig, dist, ele)

        pl.show()

    def speed_time(self, fig, time, spd):
        hours = mdates.HourLocator()
        minutes = mdates.MinuteLocator()
        hoursfmt = mdates.DateFormatter('%H:%M')

        ax = fig.add_subplot(111)
        ax.plot(time, spd)

        #pl.plot(time, ele, linewidth=1.0)
        ax.xaxis.set_label('time')
        ax.yaxis.set_label('speed (m/s)')
        ax.set_title('speed-time plot')

        # format the ticks
        ax.xaxis.set_major_locator(hours)
        ax.xaxis.set_major_formatter(hoursfmt)
        ax.xaxis.set_minor_locator(minutes)

        #datemin = datetime.date(time.min().hour, 1, 1)
        #datemax = datetime.date(r.date.max().year+1, 1, 1)
        #ax.set_xlim(datemin, datemax)

        # format the coords message box
        #def price(x): return '$%1.2f'%x
        ax.format_xdata = mdates.DateFormatter('%Y-%m-%d')
        #ax.format_ydata = price
        ax.grid(True)

        # rotates and right aligns the x labels, and moves the bottom of the
        # axes up to make room for them
        fig.autofmt_xdate()

    def profile(self, fig, dist, ele):
        cmap=plt.get_cmap('brg')
        cmap = ListedColormap(['green', 'black', 'red'])
        norm=plt.Normalize(-1500, 1500)

        #cmap = ListedColormap(['r', 'g', 'b'])
        #norm = BoundaryNorm([-1, -0.5, 0.5, 1], cmap.N)

        dx = np.gradient(dist)
        dy = np.gradient(ele)
        grad = dy / dx

        points = np.array([dist, ele]).T.reshape(-1, 1, 2)
        segments = np.concatenate([points[:-1], points[1:]], axis=1)
        lc = LineCollection(segments, cmap=cmap, norm=norm)
        lc.set_array(grad)
        lc.set_linewidth(3)

        ax = fig.add_subplot(111)

        #ax.plot(dist, ele)
        ax.xaxis.set_label('dist (km)')
        ax.yaxis.set_label('ele (m)')
        ax.xaxis.set_visible(True)
        ax.yaxis.set_visible(True)
        ax.set_title('elevation profile')
        ax.grid(True)
        ax.add_collection(lc)
        #ax.set_xlim(dist.min(), dist.max())
        #ax.set_ylim(dist.min(), dist.max())
        ax.plot(dist, ele, 'black', linestyle='None')

        #ax2 = fig.add_subplot(211)
        #ax2.plot(dist, grad)
        #ax2.set_ylim(-1200, 1200)

class Stats:
    def __init__(self, infile):
        self.infile = infile
        self.writer = None

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

        self.in_map = {}
        self.last_point = None
        self.cur_point = None

        self.p = xml.parsers.expat.ParserCreate()
        self.p.StartElementHandler = self.start_element
        self.p.EndElementHandler = self.end_element
        self.p.CharacterDataHandler = self.char_data

    def run(self, writer):
        self.writer = writer
        self.p.ParseFile(self.infile)

    def do_stats(self, cur_point, last_point):
        if last_point is not None:
            b = point.Point(cur_point.lat, cur_point.lon)
            a = point.Point(last_point.lat, last_point.lon)
            dst = b.distance(a)
            self.total_dist += dst

        self.writer.write_point(cur_point, self.total_dist)

    def end_element(self, name):
        #global in_map, cur_point, last_point, stats, cdata

        if self.in_elem("trkpt"):
            if self.in_elem("ele"):
                self.cur_point.ele = float(self.cdata)
            if self.in_elem("time"):
                self.cur_point.time_gmt = gpx_time(self.cdata)
            if self.in_elem("speed"):
                self.cur_point.speed = float(self.cdata)

        self.in_map[name] = False

        if name == "trkpt":
            self.do_stats(self.cur_point, self.last_point)

    def char_data(self, data):
        self.cdata += data

    def in_elem(self, name):
        if name in self.in_map:
            return self.in_map[name]
        return False

    def start_element(self, name, attrs):
        #global in_map, cur_point, last_point, cdata
        self.in_map[name] = True
        self.cdata = ""

        if name == "trkpt":
            self.last_point = self.cur_point
            self.cur_point = GpxPoint(float(attrs["lat"]), float(attrs["lon"]))

class GpxPoint:
    def __init__(self, lat, lon):
        self.lat = lat
        self.lon = lon
        self.speed = 0
        self.ele = 0
        self.time_gmt = gmtime(0)

def gpx_time(cdata):
    # ignore milliseconds
    FMT = "%Y-%m-%dT%H:%M:%S"
    LEN = 19
    return strptime(cdata[0:LEN], FMT)

def gmtime2datetime(x):
    return datetime(x.tm_year, x.tm_mon, x.tm_mday, x.tm_hour, x.tm_min, x.tm_sec)

def main(d, today):
    stats = Stats(open("%s.gpx" % d))
    main_dat(d, today, stats)
    #stats = Stats(open("%s.gpx" % d))
    #main_matplot(d, today, stats)

def main_dat(d, today, stats):
    datw = DatWriter(open("%s.dat" % d, "w"))
    datw.header()
    stats.run(datw)
    datw.footer()

def main_matplot(d, today, stats):
    plotter = Matplotter()
    stats.run(plotter)
    plotter.go()

if __name__ == '__main__':
    d = argv[1]
    today = date(int(d[0:4]), int(d[4:6]), int(d[6:8]))
    main(d, today)
