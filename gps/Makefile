TRACK_DIR = ../trackdata
GALLERY_DIR = $(HOME)/g2data/albums/tour
SCRIPT_PATH = ..

GPS_PREFIX = $(TRACK_DIR)/RODNEY_833000060_

TRACKCOLOUR = $(SCRIPT_PATH)/trackcolour.py
TRACKCOLOUR_GNUPLOT = $(SCRIPT_PATH)/trackcolour.py -g
TRACKCOLOUR_MAPSERVER = $(SCRIPT_PATH)/trackcolour.py -m
TRACKTITLE = $(SCRIPT_PATH)/tracktitle.py
DATE2PATH = $(SCRIPT_PATH)/date2path.py
PROCESS_GPX = $(SCRIPT_PATH)/processgpx.py
ALLDAYS = $(SCRIPT_PATH)/alldays.py
BLOGPOST = $(SCRIPT_PATH)/blogpost.py
MAKE_PHOTOS = $(SCRIPT_PATH)/makephotos.py
MAKE_MAP = $(SCRIPT_PATH)/make-map.py
GPXPHOTOS = awk -f $(SCRIPT_PATH)/gpxphotos.awk
GET_GEOTAG = $(SCRIPT_PATH)/get_geotag.php

PLOT_TEMPLATE = $(SCRIPT_PATH)/gnuplot-template.m4

# all dates custom function
DATES := $(shell $(ALLDAYS))
dates = $(foreach date,$(DATES),$(2)$(date)$(1))

ALL_GPX	      = $(call dates,.gpx)
ALL_GPX_MIN   = $(call dates,.min.gpx)
ALL_TRACK_KML = $(call dates,.kml)
ALL_LAYERS    = $(call dates,-layer.m4)
ALL_SHAPES    = $(foreach fmt,shp shx dbf,$(call dates,_trk.$(fmt)))
ALL_GRAPHS    = $(foreach type,ele ele2 spd,$(call dates,-$(type).png)) \
                empty-ele2.png

GPS_DATA := $(HOME)/GPS_DATA

links:
	make -C $(GPS_DATA)
	ln -sf $(GPS_DATA)/20??????.gpx .

all: all-kml all-graphs all-js tour.map

all-kml: $(ALL_TRACK_KML)

all-gpx: $(ALL_GPX)

all-gpx-min: $(ALL_GPX_MIN)

all-gc: $(call dates,.gc)

all-graphs: $(ALL_GRAPHS)

ALL_JS = tracks.js photos.js colours.js
all-js: $(ALL_JS)

all-shapes: $(ALL_SHAPES)

NMEA = $(wildcard $(GPS_PREFIX)$**.TXT)

%.min.gpx: %.gpx
	gpsbabel -t -i gpx -f $^ -x simplify,crosstrack,error=0.05k -o gpx -F $@

#%.gpx: %-gps.gpx
#	gpsbabel -i gpx -f $< $(foreach gpx,$(wildcard ../handmade/$*.gpx),-f $(gpx)) -o gpx -F $@

%.gpx: $(GPS_DATA)/%.gpx
#%.gpx:
	@./linkday.sh $(GPS_DATA) $@

#%-gps.gpx:
#	gpsbabel -i nmea $(foreach nmea,$(NMEA),-f $(nmea)) -x discard,sat=3 -x duplicate,location -x track,merge,sdistance=0.1k -x simplify,error=0.001k -x sort,time -o gpx -F $@ || true

%.kml: %.gpx
	gpsbabel -i gpx -f $^ -o kml,units=m,points=0,lines=1,line_width=4,line_color=$(shell $(TRACKCOLOUR) $*) -F $@
	sed -e 's=GPS device=$(shell $(TRACKTITLE) $*)=' -e 's=Path=$*=' -i $@

### graphs

# Get data from GPX
%.dat: %.gpx
	$(PROCESS_GPX) $*

# And make a gnuplot command file
%.gnuplot: $(PLOT_TEMPLATE)
	m4 -DDATE=$* -DCOLOUR=$(shell $(TRACKCOLOUR_GNUPLOT) $*) $(PLOT_TEMPLATE) > $@

.INTERMEDIATE: $(foreach type,ele ele2 spd,$(call dates,-$(type).ps))
.INTERMEDIATE: $(call dates,.gnuplot) $(call dates,.dat)

# Run gnuplot to make postscript graphs
%-ele.ps %-ele2.ps %-spd.ps: %.gnuplot %.dat
	gnuplot $*.gnuplot

# Use imagemagick to convert graphs to png
%.png: %.ps
	convert -density 100 $*.ps $*.png


all.kml: $(ALL_GPX)
	gpsbabel -i gpx $(foreach GPX,$^,-f $(GPX)) -x simplify,count=1000 -o kml,points=0,lines=1,line_width=4,line_color=c0606060,units=m -F $@

tour2012.kml: ../stops.kml $(ALL_TRACK_KML)
	gpsbabel -i kml $(foreach KML,$(ALL_TRACK_KML),-f $(KML)) -x simplify,count=1000 -o kml,points=0,lines=1,units=m -F $@

all.gpx: $(ALL_GPX) ../tour2012.csv
	gpsbabel -i gpx $(foreach GPX,$(ALL_GPX),-f $(GPX)) -x simplify,count=200 -o gpx -F - | egrep -v '<(fix|sat|hdop|course|speed)>' | sed -e 's/ 	//g' > $@

%.gc: %.gpx
	gpscorrelate -v -z +2 -g $^ ../photos/$(shell $(DATE2PATH) $*)/*.JPG
#	touch $@

blog-%: %.kml %.dat
	$(BLOGPOST) $*

photos.js: $(call dates,-photos.js)
	@echo "Building $@"
	@echo "var photos = new Array();" > $@
	@echo >> $@
	@cat $^ >> $@

%-geotag:
	$(GET_GEOTAG) $* 2> /dev/null > $@ 

%-photos.js: %-geotag
	$(MAKE_PHOTOS) $* < $^ > $@

tracks.js:
	$(MAKE_MAP) > $@

colours.js:
	$(TRACKCOLOUR) -j > $@

# ArcGIS Shapefiles
%_trk.shp %_trk.dbf %_trk.shx: %.gpx
	gpx2shp -t $^
%_wpt.shp %_wpt.dbf %_wpt.shx: %.gpx
	gpx2shp -w $^

# Mapserver MAP file generation
#.INTERMEDIATE: layers.m4 $(ALL_LAYERS)

%-layer.m4: ../layer.m4
	m4 -DM_DATE=$* \
	   -DM_NAME="$(shell $(TRACKTITLE) $*)" \
	   -DM_COLOUR="$(shell $(TRACKCOLOUR_MAPSERVER) $*)" \
	   $< > $@

layers.m4: $(ALL_LAYERS)
	cat $^ > $@

tour.map: ../map.m4 ../layer.m4 layers.m4 photos_wpt.shp $(ALL_SHAPES)
	m4 -DM_SHAPEPATH=$(shell pwd) $< > $@

# Generate a waypoint GPX from all geotags
photos.gpx: $(call dates,-geotag)
	$(GPXPHOTOS) $^ > $@

# clean targets

clean: clean-gpx clean-kml clean-gc clean-graphs clean-js clean-shapes clean-map

superclean: clean clean-geotags
	rm -f *~

clean-gpx:
	rm -f $(ALL_GPX)

clean-kml:
	rm -f $(ALL_TRACK_KML)

clean-gc:
	rm -f

clean-graphs:
	rm -f $(ALL_GRAPHS)

clean-geotags:
	rm -f $(call dates,-geotag)

clean-js:
	rm -f $(call dates,-photos.js) $(ALL_JS)

clean-shapes:
	rm -f $(ALL_SHAPES) photos_wpt.dbf photos_wpt.shp photos_wpt.shx photos.gpx

clean-map:
	rm -f tour.map $(call dates,-layer.m4) layers.m4

.PHONY: upload

upload: all-gpx-min all.gpx
	rsync *.min.gpx all.gpx rodney.id.au:public_html/tour2012
