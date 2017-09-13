<template>
  <div class="tour-map-container">
    <vl-map :zoom="zoom" :center="center"  :options="extraOptions" ref="map">
      <vl-tilelayer :url="osm.url" :attribution="osm.attribution" ref="osmLayer"></vl-tilelayer>
      <vl-tilelayer :url="mapbox.url" :params="mapbox.params" :token="mapbox.params.accessToken" :attribution="mapbox.attribution" ref="mapboxLayer"></vl-tilelayer>

      <!--
      <vl-geojson-layer v-if="allTrack" :geojson="allTrack" :options="allOptions" ref="allLayer"></vl-geojson-layer>
      <vl-geojson-layer v-if="tourTrack" :geojson="tourTrack" :options="tourOptions" ref="tourLayer"></vl-geojson-layer>
      <vl-geojson-layer v-if="dayTrack" :geojson="dayTrack" :options="dayOptions" ref="trackLayer"></vl-geojson-layer>
      -->
    </vl-map>
  </div>
</template>

<script>
import L from 'leaflet';
import Models from '@/models';
import Tracks from '@/tracks';
import mapComponents from "./map_components";

export default {
  name: "tour-map",
  props: ["date", "tourName"],
  data() {
    return {
      dayTrack: null,
      tourTrack: null,
      allTrack: null,
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
    },

    initWatches() {
      this.$watch("date", date => {
        if (date) {
          Models.loadDailyTrack(date).then(track => this.setTrack(track));
        } else {
          this.setTrack(null);
        }
      }, { immediate: true });

      this.$watch("tourName", name => {
        if (name) {
          Models.loadTourTrack(name).then(track => this.setTourTrack(track));
        } else {
          this.setTourTrack(null);
        }
      }, { immediate: true });

      this.$watch(() => !(this.date || this.tourName), showIndex => {
        if (showIndex) {
          Models.loadAllTrack(name).then(track => this.setAllTrack(track));
        } else {
          this.setAllTrack(null);
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
      this.dayTrack = track;
      if (this.dayTrack) {
        this.dayTrack = track;
        this.trackLayer = L.geoJSON(this.dayTrack, this.dayOptions).addTo(this.map);

        // cause styles to be refreshed
        if (this.tourLayer) {
          this.tourLayer.setStyle(this.tourOptions.style);
        }
      }
      this.setBounds();
    },

    setTourTrack(track) {
      if (this.tourLayer) {
        this.tourLayer.remove();
        this.tourLayer = null;
      }
      this.tourTrack = track;
      if (this.tourTrack) {
        this.tourLayer = L.geoJSON(track, this.tourOptions).addTo(this.map);

        this.tourLayer.on("click", mouseEvent => {
          const name = Tracks.featureToDate(mouseEvent.layer.feature.properties.name);
          if (name) {
            this.$emit("visit", name);
          }
        });
      }
      this.setBounds();
    },

    setAllTrack(track) {
      if (this.allLayer) {
        this.allLayer.remove();
        this.allLayer = null;
      }
      this.allTrack = track;
      if (this.allTrack) {
        this.allLayer = L.geoJSON(this.allTrack, this.allOptions).addTo(this.map)

        this.allLayer.on("click", mouseEvent => {
          const name = mouseEvent.layer.feature.properties.name;
          if (name) {
            this.$emit("tour", name);
          }
        });
      }
      this.setBounds();
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
