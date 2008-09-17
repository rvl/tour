#!/usr/bin/python

from sys import argv
from datetime import date
from data import data
from os import popen

d = argv[1]

def date_num(d):
    for n, stuff in enumerate(data):
         if d == stuff[0]:
            return n
    return None

def find_date(d):
    for stuff in data:
        if d == stuff[0]:
            return stuff
    return None

stuff = find_date(argv[1])
n = date_num(argv[1]) + 1

post = popen("../blog-post.pl", "w")

if stuff[2] is None:
    post.write("Day %d: %s\n" % (n, stuff[1]))
else:
    post.write("Day %d: %s - %s\n" % (n, stuff[1], stuff[2]))

post.write("""%s

<!--more-->

<ul>
    <li><a href="http://rodney.id.au/v/tour2008/%s/%s/">View all images for today</a></li>
</ul>

Dep , arr , dst km.

<iframe width="425" height="350" frameborder="0" scrolling="no" marginheight=\
"0" marginwidth="0" src="http://maps.google.com/maps?hl=en&amp;tab=wl&amp;q=http://rodney.id.au/~rodney/gps/%s.kml&amp;output=embed"></iframe>

<img src="http://rodney.id.au/~rodney/gps/%s-ele.png" alt="" />
""" % (argv[1], argv[1][4:6], argv[1][6:8], argv[1], argv[1]))

post.close()

# <iframe width="425" height="350" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="http://maps.google.com/maps?f=q&amp;hl=en&amp;geocode=&amp;q=http:%%2F%%2Frodney.id.au%%2F~rodney%%2Fgps%%2F%s.kml&amp;ie=UTF8&amp;t=p&amp;output=embed&amp;s=AARTsJqCGESgxlahPr8Qq3ox_6ARoFeJIA"></iframe>
