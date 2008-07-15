GPS_PREFIX = RODNEY_833000060_

TRACKCOLOUR = ../trackcolour.py
TRACKTITLE = ../tracktitle.py
DATE2PATH = ../date2path.py
PROCESS_GPX = ../processgpx.py
ALLDAYS = ../alldays.py

PYTHONPATH = $(HOME)/lib/python

DATES =          20080617 20080618 20080619 20080620 20080621         \
	20080623 20080624          20080626 20080627 20080628 20080629 \
	20080630 20080701          20080703 20080704 20080705 20080706 \
	20080707 20080708 20080709 20080710 #20080711 20080712 20080713

ALL_GPX = $(foreach date,$(DATES),$(date).gpx)
#ALL_TRACK_KML = $(foreach gpx,$(ALL_GPX),$(basename $(gpx)).kml)

ALL_TRACK_KML = $(foreach date,$(DATES),$(date).kml)

all-kml: $(ALL_TRACK_KML)

all-gpx: $(ALL_GPX)

all-gc: $(foreach date,$(DATES),$(date).gc)

all-graphs: $(foreach date,$(DATES),$(date).dat)

%.gpx: $(wildcard $(GPS_PREFIX)%*.TXT)
	gpsbabel -i nmea $(foreach nmea,$(wildcard $(GPS_PREFIX)$**.TXT),-f $(nmea)) -x duplicate,location -x track,merge,sdistance=0.1k -x simplify,error=0.001k -x sort,time -o gpx -F $@

%.kml: %.gpx
	gpsbabel -i gpx -f $^ -o kml,points=0,lines=1,line_width=4,line_color=$(shell $(TRACKCOLOUR) $*) -F $@
	sed -e 's=GPS device=$(shell $(TRACKTITLE) $*)=' -i $@

%.dat: %.gpx
	$(PROCESS_GPX) $*
	gnuplot $*.gnuplot

tour2008.kml: ../stops.kml $(ALL_TRACK_KML)
	gpsbabel -i kml $(foreach KML,$(ALL_TRACK_KML),-f $(KML)) -x simplify,count=1000 -o kml,points=0,lines=1 -F $@

%.gc: %.gpx
	gpscorrelate -v -z +2 -g $^ ../photos/$(shell $(DATE2PATH) $*)/*.JPG
#	touch $@

clean: clean-gpx clean-kml clean-gc

clean-gpx:
	rm -f $(ALL_GPX)

clean-kml:
	rm -f $(ALL_TRACK_KML)

clean-gc:
	rm -f

clean-graphs:
	rm -f $(foreach date,$(DATES),$(date).dat $(date).gnuplot $(date)-ele.png $(date)-spd.png)
