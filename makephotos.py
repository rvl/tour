#!/usr/bin/python

from data import data, pathify
from sys import stdin, stdout, stderr, argv

def print_header():
    global date
    stdout.write("""function photos%s() {
var point;
var photoIcon;
var markerOptions;
var marker;
var markers = [];

""" % date)

def print_footer():
    stdout.write("""
return markers;
}
""")

def write_untagged(filename):
    pass

def write_tagged(filename, lat, lon):
    global date

    stdout.write("""// %(filename)s
point = new GLatLng(%(lat)f, %(lon)f);
photoIcon = new GIcon(G_DEFAULT_ICON);
photoIcon.image = "http://labs.google.com/ridefinder/images/mm_20_gray.png";
//photoIcon.shadow = "http://labs.google.com/ridefinder/images/mm_20_shadow.png";
photoIcon.iconSize = new GSize(12, 20);
//photoIcon.shadowSize = new GSize(22, 20);
photoIcon.iconAnchor = new GPoint(6, 20);
photoIcon.infoWindowAnchor = new GPoint(5, 1);

markerOptions = { icon:photoIcon };
marker = new GMarker(point, markerOptions);

GEvent.addListener(marker, "click", function() {
    marker.openInfoWindowHtml("Photo <b>%(filename)s</b><br><img src=\\"%(url)s\\">");
  });

markers.push(marker);
""" % { "filename" : filename, "lat" : lat, "lon" : lon, "url" : "http://gallery.rodney.id.au/v/tour%s/%s" % (pathify(date), filename)})

def process_line(line):
    (directory, filename, lat, lon) = line.split()
    if lat == '-' or lon == '-':
        write_untagged(filename)
    else:
        write_tagged(filename, float(lat), float(lon))

date = argv[1]

print_header()

line = stdin.readline()

while line is not None and line != "":
    process_line(line)
    line = stdin.readline()

print_footer()
