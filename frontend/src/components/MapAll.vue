<template>
  <div class="tour-map-container">
    <vl-map :zoom="zoom" :bounds="bounds" :center="center"  :options="extraOptions" ref="map">
      <vl-tilelayer :url="osm.url" :attribution="osm.attribution" ref="osmLayer"></vl-tilelayer>
      <vl-tilelayer :url="mapbox.url" :params="mapbox.params" :token="mapbox.params.accessToken" :attribution="mapbox.attribution" ref="mapboxLayer"></vl-tilelayer>

      <vl-geojson-layer v-if="allTrack" :geojson="allTrack" :options="allOptions" ref="allLayer"></vl-geojson-layer>

      <router-view name="map"></router-view>
    </vl-map>
  </div>

</template>

<script>
import TourMap from '@/components/TourMap';

export default {
  name: "map-all",
  data() {
    return {
      dayTrack: null,
      tourTrack: null,
      allTrack: null,
      dayOptions: {
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
      allOptions: {
        style: geoJsonFeature => ({
          weight: 3,
          color: 'red',
          opacity: 1,
          fillColor: '#e4ce7f',
          fillOpacity: 1
        })
      },
      extraOptions: {
      },
      zoom: 6,
      bounds: null,
      center: [0, 0],
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
  components: {
    TourMap
  }
}
</script>
