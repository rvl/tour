<template>
  <div class="tour-map-container">
    <vl-map :zoom="zoom" :center="center"  :options="extraOptions" ref="map">
      <vl-tilelayer :url="osm.url" :attribution="osm.attribution" ref="osmLayer"></vl-tilelayer>
      <vl-tilelayer :url="mapbox.url" :params="mapbox.params" :token="mapbox.params.accessToken" :attribution="mapbox.attribution" ref="mapboxLayer"></vl-tilelayer>
    </vl-map>
  </div>
</template>

<script>
import L from 'leaflet';
import Models from '@/models';
import Tracks from '@/tracks';
import mapComponents from "./map_components";
import Filters from "@/filters";

export default {
  name: "tour-map",
  props: ["date", "tourName"],
  data() {
    return {
      track: {
        day: null,
        tour: null,
        all: null
      },
      info: {
        index: null,
        tour: null
      },
      dayOptions: {
        style: geoJsonFeature => ({
          weight: 3,
          color: '#729fcf',
          opacity: 1
        })
      },
      tourOptions: {
        style: geoJsonFeature => ({
          stroke: !this.date || geoJsonFeature.properties.name != this.featureName,
          weight: 3,
          color: '#c17d11',
          opacity: 1
        })
      },
      allOptions: {
        style: geoJsonFeature => ({
          weight: 3,
          color: '#a40000',
          opacity: 1
        })
      },
      extraOptions: {
      },
      zoom: 6,
      center: [-31.952222, 115.858889],
      osm: {
        url: "http://{s}.tile.osm.org/{z}/{x}/{y}.png",
        attribution: `Map data &copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors`
      },
      mapbox: {
        url: "https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}",
        params: {
          id: "mapbox.streets",
          accessToken: "pk.eyJ1IjoicnZsIiwiYSI6ImMzNzdiNWQ1YTMzYTRjNzEyOTU2ZTY2NDhiNTQ5MDBhIn0.out7-ubBjWy-7C_FH4WUHQ"
        },
        attribution: `Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://mapbox.com">Mapbox</a>`
      }
    }
  },
  methods: {
    initMap() {
      this.map = this.$refs.map.mapObject;
      this.osmLayer = this.$refs.osmLayer.mapObject;
      this.mapboxLayer = this.$refs.mapboxLayer.mapObject;

      L.control.layers({
        "OSM Mapnik": this.osmLayer,
        "OSM Mapbox Streets": this.mapboxLayer
      }).addTo(this.map);

      this.tourLayer = this.$refs.tourLayer ? this.$refs.tourLayer.$geoJSON : null;
      this.trackLayer = this.$refs.trackLayer ? this.$refs.trackLayer.$geoJSON : null;

      this.map.on("zoomend", ev => this.updateVisibilityZoom());

      this.map.on("click", ev => {
        console.log(`coord: [${ev.latlng.lng}, ${ev.latlng.lat}]`);
      });
    },

    initWatches() {
      this.$watch("date", date => {
        if (date) {
          Models.track.daily(date).then(track => this.setTrack(track));
        } else {
          this.setTrack(null);
        }
      }, { immediate: true });

      this.$watch("tourName", name => {
        if (name) {
          Models.track.tour(name).then(track => this.setTourTrack(track));
          Models.info.tour(name).then(tour => this.setTourInfo(tour));
        } else {
          this.setTourTrack(null);
          this.setTourInfo(null);
        }
      }, { immediate: true });

      this.$watch(() => !(this.date || this.tourName), showIndex => {
        if (showIndex) {
          Models.track.all().then(track => this.setAllTrack(track));
          Models.info.index().then(info => { this.info.index = info; });
        } else {
          this.setAllTrack(null);
          this.info.index = null;
        }
      }, { immediate: true });
    },

    setBounds() {
      if (this.trackLayer) {
        this.map.fitBounds(this.trackLayer.getBounds());
      } else if (this.tourLayer) {
        this.map.fitBounds(this.tourLayer.getBounds());
      } else if (this.allLayer) {
        this.map.fitBounds(this.allLayer.getBounds());
      }
    },

    setTrack(track) {
      if (this.trackLayer) {
        this.trackLayer.remove();
        this.trackLayer = null;
      }
      this.track.day = track;
      if (this.track.day) {
        this.track.day = track;
        this.trackLayer = L.geoJSON(this.track.day, this.dayOptions).addTo(this.map);

        // cause styles to be refreshed
        if (this.tourLayer) {
          this.tourLayer.setStyle(this.tourOptions.style);
        }

        // show a tooltip ... alas getting the elevation is difficult
        this.trackLayer.bindTooltip(layer => this.tourDayTooltip(layer.feature.properties.name), { sticky: true });
      }
      this.setBounds();
    },

    setTourTrack(track) {
      if (this.tourLayer) {
        this.tourLayer.remove();
        this.tourLayer = null;
      }
      this.track.tour = track;
      if (this.track.tour) {
        this.tourLayer = L.geoJSON(track, this.tourOptions).addTo(this.map);

        // click event -- for navigation usually
        this.tourLayer.on("click", mouseEvent => {
          const name = Tracks.featureToDate(mouseEvent.layer.feature.properties.name);
          if (name) {
            this.$emit("visit", name);
          }
        });

        // Show track info on tooltip hover
        this.tourLayer.bindTooltip(layer => this.tourDayTooltip(layer.feature.properties.name), { sticky: true });
      }
      this.setBounds();
    },

    setAllTrack(track) {
      if (this.allLayer) {
        this.allLayer.remove();
        this.allLayer = null;
      }
      this.track.all = track;
      if (this.track.all) {
        this.allLayer = L.geoJSON(this.track.all, this.allOptions).addTo(this.map)

        this.allLayer.on("click", mouseEvent => {
          const name = mouseEvent.layer.feature.properties.name;
          if (name) {
            this.$emit("tour", name);
          }
        });

        // Show tour info on tooltip hover
        this.allLayer.bindTooltip(layer => this.tourTooltip(layer.feature.properties.name), { sticky: true });
      }
      this.setBounds();
    },

    setTourInfo(info) {
      if (this.infoLayer) {
        this.infoLayer.remove();
        this.infoLayer = null;
      }
      this.info.tour = info;
      if (info) {
        const icon = colour => L.icon({
          iconUrl: `${Models.staticUrl}images/marker-icon-${colour}.png`,
          iconRetinaUrl: `${Models.staticUrl}images/marker-icon-2x-${colour}.png`,
          iconSize: [25, 41],
          iconAnchor: [13, 41],
          popupAnchor: [0, -10]
        });
        const blueIcon = icon("blue");
        const greenIcon = icon("green");
        const redIcon = icon("red");

        const popup = (day, where, end) => {
          return `Day ${day.num}. ${Filters.formatDate(day.date)}<br>${where}`
        };

        const markers = _.map(info.days, day => {
          const coord = day.from_coord || day.to_coord;
          return coord ?
            L.marker(coord, {
              icon: blueIcon,
              title: day.from || day.to
            }).bindPopup(popup(day, day.from || day.to, null)) : null;
        }).filter(m => m).valueOf();

        this.infoTourLayer = L.layerGroup(_.filter(markers));
        this.infoLayers = _(info.days).map(day => [
          day.date,
          L.layerGroup([
            day.from_coord ?
              L.marker(day.from_coord, {
                icon: greenIcon,
                title: day.from,
                zIndexOffset: 1000
              }).bindPopup(popup(day, day.from, false)) : null,
            day.to_coord ?
              L.marker(day.to_coord, {
                icon: redIcon,
                title: day.to,
                zIndexOffset: 1000
              }).bindPopup(popup(day, day.to, true)) : null
          ].filter(m => m))
        ]).fromPairs().valueOf();
        this.updateVisibilityZoom();
      }
    },

    updateVisibilityZoom() {
      if (this.infoTourLayer) {
        if (this.map.getZoom() >= 8) {
          this.map.addLayer(this.infoTourLayer);
        } else {
          this.map.removeLayer(this.infoTourLayer);
        }
      }
      _.each(this.infoLayers, (layer, date) => {
        if (this.date === date) {
          this.map.addLayer(layer);
        } else {
          this.map.removeLayer(layer);
        }
      });
    },

    // tooltip text for a segment of tour track
    tourDayTooltip(featureName) {
      const date = Tracks.featureToDate(featureName);
      if (date && this.info.tour && this.info.tour.days) {
        const day = _.find(this.info.tour.days, { date });
        if (day) {
          return `Day ${day.num}. ${Filters.formatDate(date)}<br>${Filters.formatDayTitle(day)}`;
        }
      }
      return `${date}`;
    },

    // tooltip text for a segment of all tours track
    tourTooltip(featureName) {
      if (this.info.index && this.info.index[featureName]) {
        return this.info.index[featureName].name;
      }
      return featureName;
    }
  },
  mounted() {
    this.initMap();
    this.initWatches();
  },
  computed: {
    featureName() {
      return Tracks.dateToFeature(this.date);
    }
  },
  components: mapComponents
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style lang="scss" scoped>
@import "~leaflet/dist/leaflet.css";

.tour-map-container {
}
</style>
