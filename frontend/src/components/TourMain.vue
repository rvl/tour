<template>
  <div class="tour-main">
    <tour-map class="map-view" :tour-name="tourName" :date="tourDate"
              @visit="onVisit" @tour="onTour"></tour-map>

    <div class="main-view">
      <router-view></router-view>
    </div>
  </div>
</template>

<script>
import TourMap from '@/components/TourMap';

export default {
  name: "tour-main",
  data() {
    return {
      tourName: this.$router.currentRoute.params.name || "",
      tourDate: this.$router.currentRoute.params.date || ""
    };
  },
  created() {
    this.$router.beforeEach((to, from, next) => {
      this.tourName = to.params.name || "";
      this.tourDate = to.params.date || "";
      next();
    });
  },
  methods: {
    onVisit(date) {
      this.$router.push({ name: "TourDay", params: { "date": date, "name": this.tourName } });
    },
    onTour(name) {
      console.log("change tour", name);
      this.$router.push({ name: "TourPageIndex", params: { "name": name } });
    }
  },
  components: {
    TourMap
  }
}
</script>

<style lang="scss" scoped>
.map-view {
  height: 100vh;
  width: 100%;
}

.main-view {
  position: absolute;
  z-index: 1000;
  pointer-events: none;
  top: 0px;
  left: 0px;
  bottom: 0px;
  right: 0px;

  & > div {
    pointer-events: all;
  }
}

</style>
