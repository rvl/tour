GPS_PREFIX = RODNEY_833000060_

GALLERY_DIR = $(HOME)/g2data/albums/tour

SCRIPT_PATH = ..

TRACKCOLOUR = $(SCRIPT_PATH)/trackcolour.py
TRACKCOLOUR_GNUPLOT = $(SCRIPT_PATH)/trackcolour.py -g
TRACKTITLE = $(SCRIPT_PATH)/tracktitle.py
DATE2PATH = $(SCRIPT_PATH)/date2path.py
PROCESS_GPX = $(SCRIPT_PATH)/processgpx.py
ALLDAYS = $(SCRIPT_PATH)/alldays.py
BLOGPOST = $(SCRIPT_PATH)/blogpost.py
MAKE_PHOTOS = $(SCRIPT_PATH)/makephotos.py
MAKE_MAP = $(SCRIPT_PATH)/make-map.py

PLOT_TEMPLATE = $(SCRIPT_PATH)/gnuplot-template.m4

PYTHONPATH = $(HOME)/lib/python

DATES = $(shell $(ALLDAYS))

ALL_GPX = $(foreach date,$(DATES),$(date).gpx)
#ALL_TRACK_KML = $(foreach gpx,$(ALL_GPX),$(basename $(gpx)).kml)

ALL_TRACK_KML = $(foreach date,$(DATES),$(date).kml)

all: all-kml all-graphs all-js

all-kml: $(ALL_TRACK_KML)

all-gpx: $(ALL_GPX)

all-gc: $(foreach date,$(DATES),$(date).gc)

ALL_GRAPHS = $(foreach date,$(DATES),$(date)-ele.png $(date)-ele2.png $(date)-spd.png)

all-graphs: $(ALL_GRAPHS)

ALL_JS = tracks.js photos.js
all-js: $(ALL_JS)

%.gpx: $(wildcard $(GPS_PREFIX)%*.TXT)
	gpsbabel -i nmea $(foreach nmea,$(wildcard $(GPS_PREFIX)$**.TXT),-f $(nmea)) -x discard,sat=3 -x duplicate,location -x track,merge,sdistance=0.1k -x simplify,error=0.001k -x sort,time -o gpx -F $@

%.kml: %.gpx
	gpsbabel -i gpx -f $^ -o kml,points=0,lines=1,line_width=4,line_color=$(shell $(TRACKCOLOUR) $*),units=m -F $@
	sed -e 's=GPS device=$(shell $(TRACKTITLE) $*)=' -e 's=Path=$*=' -i $@

### graphs

# Get data from GPX
%.dat: %.gpx
	$(PROCESS_GPX) $*

# And make a gnuplot command file
%.gnuplot: $(PLOT_TEMPLATE)
	m4 -DDATE=$* -DCOLOUR=$(shell $(TRACKCOLOUR_GNUPLOT) $*) $(PLOT_TEMPLATE) > $@

# Run gnuplot to make postscript graphs
%-ele.ps %-ele2.ps %-spd.ps: %.gnuplot %.dat
	gnuplot $*.gnuplot

# Use imagemagick to convert graphs to png
%.png: %.ps
	convert -density 100 $*.ps $*.png


all.kml: $(ALL_GPX)
	gpsbabel -i gpx $(foreach GPX,$^,-f $(GPX)) -x simplify,count=1000 -o kml,points=0,lines=1,line_width=4,line_color=c0606060,units=m -F $@

tour2008.kml: ../stops.kml $(ALL_TRACK_KML)
	gpsbabel -i kml $(foreach KML,$(ALL_TRACK_KML),-f $(KML)) -x simplify,count=1000 -o kml,points=0,lines=1,units=m -F $@

%.gc: %.gpx
	gpscorrelate -v -z +2 -g $^ ../photos/$(shell $(DATE2PATH) $*)/*.JPG
#	touch $@

blog-%: %.kml %.dat
	$(BLOGPOST) $*

photos.js: $(foreach date,$(DATES),$(date)-photos.js)
	cat $^ > $@

%-geotag: %.gpx
	exiftool -t -S -q -f -n -Directory -FileName -GpsLatitude -GpsLongitude $(GALLERY_DIR)$(shell echo $* | sed -e 's=\(....\)\(..\)\(..\)=\1/\2/\3=') > $@

%-photos.js: %-geotag
	$(MAKE_PHOTOS) $* < $^ > $@

tracks.js:
	$(MAKE_MAP) > $@

clean: clean-gpx clean-kml clean-gc clean-graphs clean-js

clean-gpx:
	rm -f $(ALL_GPX)

clean-kml:
	rm -f $(ALL_TRACK_KML)

clean-gc:
	rm -f

clean-graphs:
	rm -f $(ALL_GRAPHS)

clean-js:
	rm -f $(foreach date,$(DATES),$(date)-photos.js) $(ALL_JS)
