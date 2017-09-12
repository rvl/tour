<template>
  <div class="tour-map-container">
    <vl-map :zoom="zoom" :bounds="bounds" :options="extraOptions" ref="map">
      <vl-tilelayer :url="osm.url" :attribution="osm.attribution" ref="osmLayer"></vl-tilelayer>
      <vl-tilelayer :url="mapbox.url" :params="mapbox.params" :token="mapbox.params.accessToken" :attribution="mapbox.attribution" ref="mapboxLayer"></vl-tilelayer>
      <!-- <vl-marker :lat-lng="[47.413220, -1.219482]"></vl-marker> -->
      <vl-geojson-layer :geojson="tour" :options="tourOptions" ref="tourLayer"></vl-geojson-layer>
      <vl-geojson-layer :geojson="track" :options="trackOptions" ref="trackLayer"></vl-geojson-layer>
    </vl-map>
  </div>
</template>

<script>
import Vue2Leaflet from 'vue2-leaflet';
import L from 'leaflet';
import Models from '@/models';
import Tracks from '@/tracks';

export default {
  name: "tour-map",
  props: ["date", "tourName"],
  data() {
    return {
      track: null,
      tour: null,
      trackOptions: {
        style: geoJsonFeature => ({
          weight: 3,
          color: 'red',
          opacity: 1,
          fillColor: '#e4ce7f',
          fillOpacity: 1
        })
      },
      tourOptions: {
        style: geoJsonFeature => ({
          stroke: !this.date || geoJsonFeature.properties.name != this.featureName,
          weight: 3,
          color: '#888888',
          opacity: 1,
          fillColor: '#e4ce7f',
          fillOpacity: 1
        })
      },
      extraOptions: {
      },
      zoom: 6,
      bounds: null,
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
    load(date) {
      Models.loadDailyTrack(date).then(track => {
        this.track = track;
        this.bounds = L.geoJSON(this.track).getBounds();

        // cause styles to be refreshed
        this.tourLayer.setStyle(this.tourOptions.style);
      });
    }
  },
  computed: {
    featureName() {
      return Tracks.dateToFeature(this.date);
    }
  },
  mounted() {
    this.map = this.$refs.map.mapObject;
    this.osmLayer = this.$refs.osmLayer.mapObject;
    this.mapboxLayer = this.$refs.mapboxLayer.mapObject;
    this.tourLayer = this.$refs.tourLayer.$geoJSON;
    this.trackLayer = this.$refs.trackLayer.$geoJSON;

    L.control.layers({
      "OSM Mapnik": this.osmLayer,
      "OSM Mapbox Streets": this.mapboxLayer
    }).addTo(this.map);

    this.tourLayer.on("click", mouseEvent => {
      const name = Tracks.featureToDate(mouseEvent.layer.feature.properties.name);
      if (name) {
        this.$emit("visit", name);
      }
    });
  },
  created() {
    this.$watch("date", date => {
      if (date) {
        this.load(date);
      }
    }, { immediate: true });

    this.$watch("tourName", name => {
      if (name) {
        Models.loadTourTrack(name).then(track => {
          this.tour = track;
        });
      }
    }, { immediate: true });
  },
  components: {
    "vl-map": Vue2Leaflet.Map,
    "vl-tilelayer": Vue2Leaflet.TileLayer,
    'vl-geojson-layer': Vue2Leaflet.GeoJSON,
    "vl-marker": Vue2Leaflet.Marker
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style lang="scss" scoped>
@import "~leaflet/dist/leaflet.css";

.tour-map-container {
  height: 480px;
  width: 100%
}
</style>
