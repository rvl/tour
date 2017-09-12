#!/usr/bin/python

from data import data

print " ".join([day.strdate() for day in data if day.gpx_exists()])
