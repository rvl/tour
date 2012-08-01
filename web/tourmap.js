
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
    if (window.google) {
      layers.gphy = new OpenLayers.Layer.Google(
        "Google Physical",
        {type: google.maps.MapTypeId.TERRAIN}
      );
      layers.gsat = new OpenLayers.Layer.Google(
        "Google Satellite",
        {type: google.maps.MapTypeId.SATELLITE, numZoomLevels: 22}
      );
    }
    layers.track = create_track_layer("Track", TRACK_URL);
    layers.stops = create_json_layer("Stops", JSON_PATH);

    $.each(layers, function(key, layer) {
      map.addLayer(this);
    });

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
      strokeColor: "#000080",
      strokeOpacity: 0.8,
      strokeWidth: 5,
      //pointerEvents: "visiblePainted",
      // label with \n linebreaks
      //label : "${accom}",
      //fontColor: "${favColor}",
      fontSize: "12px",
      //fontFamily: "Courier New, monospace",
      fontWeight: "bold",
      //labelAlign: "${align}",
      //labelXOffset: "${xOffset}",
      //labelYOffset: "${yOffset}",
      labelOutlineColor: "white",
      labelOutlineWidth: 3
    }, { context: context });

    var layer = new OpenLayers.Layer.Vector(name, {
      strategies: [new OpenLayers.Strategy.Fixed()],
      styleMap: new OpenLayers.StyleMap(style),
      //style: {strokeColor: "#000080", strokeWidth: 5, strokeOpacity: 0.8},
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

  var create_json_layer = function(name, url) {
    var popup = null;
    var layer = new OpenLayers.Layer.Vector(name, {
      //styleMap: new OpenLayers.StyleMap(style),
      //style: {strokeColor: "#000080", strokeWidth: 5, strokeOpacity: 0.8},
      projection: new OpenLayers.Projection("EPSG:4326"),
      eventListeners: {
        "featureselected": function(evt) {
          var feature = evt.feature;
          var day = feature.attributes;
          var html = "Day " + day.number + ": " + day.start + " → " + day.finish + "<br/>" +
            day.dayname + " " + day.date.getDate() + "/" + (day.date.getMonth() + 1) + 
            ", dep. " + day.start_time + ", arr. " + day.finish_time + "<br/>" +
            day.dist + "km";
          popup = new OpenLayers.Popup.FramedCloud("chicken", 
                                                   feature.geometry.getBounds().getCenterLonLat(),
                                                   null, html, null, true, function(evt) {
                                                     select.unselect(feature);
                                                   });
          feature.popup = popup;
          map.addPopup(popup);
        },
        "featureunselected": function(evt) {
          var feature = evt.feature;
          if (feature.popup && feature.popup.blocks) {
            map.removePopup(feature.popup);
            feature.popup.destroy();
          }
          feature.popup = null;
        }
      }
    });
    var select = new OpenLayers.Control.SelectFeature(layer);
    map.addControl(select);
    select.activate();

    var show_day = function(date, day) {
      if (day.lon && day.lat) {
        var get_label = function(day) {
          if (day.number > 0) {
            return "Day " + day.number + ": " + day.dayname.substr(0,3) + " " + day.date.getDate() + "/" + (day.date.getMonth() + 1) + "\n" + day.finish;
          } else if (!day.prev || day.prev.finish !== day.finish) {
            return day.dayname.substr(0,3) + " " + day.date.getDate() + "/" + (day.date.getMonth() + 1) + "\n" + day.finish;
          } else {
            return "";
          }
        };
        var get_graphic = function(day) {
          return "";
          if (day.accom === "camping") {
            return "camping.png";
          } else if (day.accom === "pension" || day.accom === "pension" || day.accom === "hotel") {
            return "hotel.png";
          } else if (day.accom === "hostel") {
            return "marker-blue.png";
          } else {
            return "";
          }
        };
        var style = {
          strokeColor: "#FF0000",
          strokeOpacity: 1,
          strokeWidth: 2,
          fillColor: "#800000",
          fillOpacity: 0.8,
          pointRadius: 6,
          pointerEvents: "visiblePainted",
          cursor: "pointer",
          label: get_label(day),
          graphicName: "circle",
          externalGraphic: get_graphic(day),
          graphicWidth: 24,
          graphicHeight: 24,
          graphicXOffset: -12,
          graphicYOffset: -12,
          //fontColor: "${favColor}",
          fontSize: "12px",
          fontFamily: "Courier New, monospace",
          fontWeight: "bold",
          labelAlign: "lm",
          labelXOffset: "8",
          labelYOffset: "0",
          labelOutlineColor: "white",
          labelOutlineWidth: 3
        };
        var point = new OpenLayers.Geometry.Point(day.lon, day.lat).transform(
          layer.projection,
          map.getProjectionObject()
        );
        var feature = new OpenLayers.Feature.Vector(point, day, style);
        if (!day.prev || day.prev.finish !== day.finish) {
          layer.addFeatures([feature]);
        }
      }
    };

    var load_data = function(tour) {
      var prev = null;
      $.each(tour, function(date, info) {
        info.prev = prev;
        info.date = new Date(parseInt(date.substr(0,4)), parseInt(date.substr(4,2)) - 1, parseInt(date.substr(6,2)));
        info.dayname = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"][info.date.getDay()];
        show_day(date, info);
        prev = info;
      });
    };

    $.ajax(url)
      .success(function(data) {
        tour = data;
        load_data(data);
      });

    return layer;
  };
  var resize_map = function(id) {
    //$("#" + id).width(600).height(600);
  };

  return {
    whole_tour: function(id) {
      $(document).ready(function() {
        setup_map(id);
      });
    },
    day: function(id, date) {
      $(document).ready(function() {
        setup_map(id);
      });
    }
  };
})(jQuery);

TM.whole_tour("tour-map");
