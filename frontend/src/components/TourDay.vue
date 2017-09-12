<template>
  <div class="tour-page-index">
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
      <router-link :to="{ name: 'TourPage', params: { name: $parent.name } }">
        {{ $parent.name }}
      </router-link>
      /
      {{ date }}
    </p>

    <tour-map :tour-name="$parent.name" :date="date" @visit="onVisit"></tour-map>

    <elev-chart :date="date"></elev-chart>
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

<style scoped>
</style>
