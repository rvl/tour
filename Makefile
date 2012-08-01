all: tracks tour2012.json

tracks:
	$(MAKE) -C gps

tour2012.json: tracks tour2012.csv
	./makejson.py $@

.PHONY: tracks

clean:
	rm -f tour2012.json
#	$(MAKE) -C gps clean
