all: tracks tour2012.json

tracks:
	$(MAKE) -C gps

tour2012.json: tracks data.py
	./makejson.py $@

.PHONY: tracks

clean:
	rm -f tour2012.json
#	$(MAKE) -C gps clean
