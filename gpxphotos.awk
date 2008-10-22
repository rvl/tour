BEGIN {
    print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
    print "<gpx";
    print "version=\"1.0\"";
    print "creator=\"gpxphotos.awk\"";
    print "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"";
    print "xmlns=\"http://www.topografix.com/GPX/1/0\"";
    print "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">";
}


$3 != "-" {
    print "<wpt lat=\"" $3 "\" lon=\"" $4 "\"></wpt>";
}

END {
    print "</gpx>";
}
