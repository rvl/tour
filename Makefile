all: tracks tour2012.json

tracks:
	$(MAKE) -C gps all.gpx

tour2012.json: tracks tour2012.csv
	./makejson.py $@

upload: tour2012.json web/index.html web/index2.html web/tourmap.js
	rsync -av gps/all.gpx gps/*-ele2.png tour2012.json web/tourmap.js web/index.html web/index2.html web/jquery-1.7.2.min.js rodney.id.au:public_html/tour2012

.PHONY: tracks upload

clean:
	rm -f tour2012.json
#	$(MAKE) -C gps clean
