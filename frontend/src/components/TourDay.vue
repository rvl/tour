<template>
  <div class="tour-day">
    <div class="tour-day-popup">
      <h1>
        <span class="day-from" v-if="tourDay && tourDay.from">
          {{ tourDay.from }}
        </span>
        <span class="day-to" v-if="tourDay && tourDay.from && tourDay.to">
          → {{ tourDay.to }}
        </span>
        <span v-if="!tourDay || (!tourDay.from && !tourDay.to)">
          this day on tour
        </span>
      </h1>

      <p>
        <router-link :to="{ name: 'TourList' }">
          Tour List
        </router-link>
        /
        <router-link :to="{ name: 'TourPageIndex', params: { name: $parent.name } }">
          {{ $parent.name }}
        </router-link>
        /
        {{ date }}
      </p>
    </div>

    <div class="chart-bottom">
      <elev-chart :date="date"></elev-chart>
    </div>
  </div>
</template>

<script>
import Models from '@/models';
import TourMap from '@/components/TourMap';
import ElevChart from '@/components/ElevChart';

export default {
  name: "tour-day",
  props: ["date"],
  data () {
    return {
      tourDay: null
    }
  },
  methods: {
    onVisit(date) {
      this.$router.push({ name: "TourDay", params: { name: this.$parent.name, date } });
    }
  },
  created() {
    this.$watch("date", date => {
      if (date) {
        Models.loadTour().then(tour => {
          this.tourDay = tour.days[date];
        });
      } else {
        this.tourDay = null;
      }
    }, { immediate: true });
  },
  components: {
    TourMap,
    ElevChart
  }
}
</script>

<style lang="scss" scoped>
h1 {
  margin: 0px;
}

.tour-day-popup {
  position: absolute;
  top: 1em;
  left: 4em;
  max-height: 90vh;
  border: 2px solid black;
  border-radius: 8px;
  box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.4);
  background: rgba(255, 255, 255, 0.8);

  padding: 0.2em 0.5em;
  z-index: 1000;
}

.chart-bottom {
  position: absolute;
  bottom: 0px;
  left: 0px;
  right: 0px;
  padding: 0.5em 1em 1em 1em;

  // background: white;
  background-image: linear-gradient(top, rgba(255,255,255,0) 35%,rgba(255,255,255,1) 100%);

  pointer-events: none;
}
</style>
