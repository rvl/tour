#!/usr/bin/python

from data import data

PREFIX = "http://rodney.id.au/~rodney/gps/"

print "var trackOverlays = new Array(%d);" % len(data)

print "function addTrackOverlays() {"
for i, day in enumerate(data):
    print "  trackOverlays[%d] = new GGeoXml(\"%s%s.kml\");" % (i, PREFIX, day[0])
    print "  map.addOverlay(trackOverlays[%d]);" % i
print "}"

print "function addPhotos() {"
for day in data:
    print "  mgr.addMarkers(photos%s(), 9);" % day[0]
print "  mgr.refresh();"
print "}"
