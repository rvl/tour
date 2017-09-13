<template>
  <vl-geojson-layer v-if="tourTrack" :geojson="tourTrack" :options="tourOptions" ref="tourLayer"></vl-geojson-layer>
</template>

<script>
import TourMap from '@/components/TourMap';

export default {
  name: "map-tour",
  data() {
    return {
      tourTrack: null
    };
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
  created() {
    this.$watch(() => this.$parent.name, name => {
      console.log("MapTour create " + name);
      if (name) {
      console.log("name is " + name);
        Models.loadTourTrack(name).then(track => {
          console.log("loaded track", track);
          this.tourTrack = track;
        });
      }
    }, { immediate: true });


  },
  components: {
    TourMap
  }
}
</script>
