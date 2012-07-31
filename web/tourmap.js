
var TM = (function($) {
  var DATA_ROOT = "/~rodney/tour2012/";
  var JSON_PATH = DATA_ROOT + "tour2012.json";
  var TRACK_URL = DATA_ROOT + "all.gpx";
  var map;
  var tour;
  var layers = {};

  var setup_map = function(id) {
    resize_map(id);

    map = new OpenLayers.Map(id, {
      controls: [
        new OpenLayers.Control.Navigation(),
        new OpenLayers.Control.LayerSwitcher(),
        new OpenLayers.Control.PanZoomBar(),
        new OpenLayers.Control.ScaleLine(),
        //new OpenLayers.Control.MousePosition(),
        new OpenLayers.Control.KeyboardDefaults()
      ]
    });

    layers.osm = new OpenLayers.Layer.OSM("OpenStreetMap Mapnik");
    //layers.mq = new OpenLayers.Layer.OSM("Mapquest (OSM)");
    var mqattr = "Tiles Courtesy of <a href='http://open.mapquest.co.uk/' target='_blank'>MapQuest</a> <img src='http://developer.mapquest.com/content/osm/mq_logo.png' width='16' height='16' border='0'>";

    layers.mq = new OpenLayers.Layer.OSM("Mapquest (OSM)",
                                         ["http://otile1.mqcdn.com/tiles/1.0.0/osm/${z}/${x}/${y}.jpg",
                                          "http://otile2.mqcdn.com/tiles/1.0.0/osm/${z}/${x}/${y}.jpg",
                                          "http://otile3.mqcdn.com/tiles/1.0.0/osm/${z}/${x}/${y}.jpg",
                                          "http://otile4.mqcdn.com/tiles/1.0.0/osm/${z}/${x}/${y}.jpg"],
                                         {
                                           layerCode: "Q",
                                           //minZoomLevel: 4,
                                           //maxZoomLevel: MAX_ZOOM,
                                           attribution: mqattr,
                                           transitionEffect: "resize"
                                         });

    layers.cycle = new OpenLayers.Layer.OSM("OpenCycleMap",
                                            ["http://a.tile.opencyclemap.org/cycle/${z}/${x}/${y}.png",
                                             "http://b.tile.opencyclemap.org/cycle/${z}/${x}/${y}.png",
                                             "http://c.tile.opencyclemap.org/cycle/${z}/${x}/${y}.png"]);
    layers.gphy = new OpenLayers.Layer.Google(
        "Google Physical",
        {type: google.maps.MapTypeId.TERRAIN}
    );
    layers.gsat = new OpenLayers.Layer.Google(
        "Google Satellite",
        {type: google.maps.MapTypeId.SATELLITE, numZoomLevels: 22}
    );
    layers.track = create_track_layer("Track", TRACK_URL);

    map.addLayers([layers.mq, layers.cycle, layers.osm,
                   layers.gphy, layers.gsat,
                   layers.track]);

    // Google.v3 uses EPSG:900913 as projection, so we have to
    // transform our coordinates
    map.setCenter(new OpenLayers.LonLat(10.2, 48.9).transform(
        new OpenLayers.Projection("EPSG:4326"),
        map.getProjectionObject()
    ), 5);

    return map;
  };

  var create_track_layer = function(name, url) {
    var context = {};
    var style = new OpenLayers.Style({
      strokeColor: "#00FF00",
      strokeOpacity: 1,
      strokeWidth: 3,
      fillColor: "#FF5500",
      fillOpacity: 0.5,
      pointRadius: 6,
      pointerEvents: "visiblePainted",
      // label with \n linebreaks
      label : "name: ${name}\n\nage: ${age}",
      fontColor: "${favColor}",
      fontSize: "12px",
      fontFamily: "Courier New, monospace",
      fontWeight: "bold",
      labelAlign: "${align}",
      labelXOffset: "${xOffset}",
      labelYOffset: "${yOffset}",
      labelOutlineColor: "white",
      labelOutlineWidth: 3
    }, { context: context });

    var layer = new OpenLayers.Layer.Vector(name, {
      strategies: [new OpenLayers.Strategy.Fixed()],
      //styleMap: new OpenLayers.StyleMap(style),
      style: {strokeColor: "#000080", strokeWidth: 5, strokeOpacity: 0.8},
      protocol: new OpenLayers.Protocol.HTTP({
	url: url,
	format: new OpenLayers.Format.GPX()
      }),
      projection: new OpenLayers.Projection("EPSG:4326"),
      eventListeners: {
        "loadend": function(event) {
          map.zoomToExtent(event.object.getDataExtent());
        }
      }
    });

    return layer;
  };

  var resize_map = function(id) {
    //$("#" + id).width(600).height(600);
  };

  var load_data = function(success) {
    $.ajax(JSON_PATH)
      .success(function(data) {
        tour = data;
        if (success) {
          success(data);
        }
      });
  };

  var show_day = function(date) {
    /*
    var newPoint = new OpenLayers.Geometry.Point(point.x + (r * Math.cos(a)),
                                                 point.y + (r * Math.sin(a)));
    var polygonFeature = new OpenLayers.Feature.Vector(newPoint);
    vectorLayer.addFeatures([pointFeature, pointFeature3, pointFeature2, lineFeature, polygonFeature]);
    */
  };

  return {
    whole_tour: function(id) {
      $(document).ready(function() {
        setup_map(id);
        load_data(function(tour) {
          $.each(tour, function(date, info) {
            show_day(date);
          });
        });
      });
    },
    day: function(id, date) {
      $(document).ready(function() {
        setup_map(id);
        load_data(function(tour) {
          show_day(date);
        });
      });
    }
  };
})(jQuery);

TM.whole_tour("tour-map");
