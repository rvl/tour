<template>
  <div class="tour-page-index">
    <h1>{{ tour.name }}</h1>
    
    <p>
      <router-link :to="{ name: 'TourList' }">
        Back to tours
      </router-link>
    </p>

    <p v-if="tour.description">
      {{ tour.description }}
    </p>

    <table class="tour-index">
      <thead>
        <th>Date</th>
        <th>From</th>
        <th>To</th>
        <th>Distance</th>
        <th></th>
      </thead>
      <tbody v-if="loading" class="loading">
        <tr colspan="5"><td>Loading...</td></tr>
      </tbody>

      <tbody v-else>
        <tr v-for="d in tour.days">
          <td>
            <router-link :to="{ name: 'TourDay', params: { date: d.date } }">
              {{ d.date | formatDate }}
            </router-link>
          </td>
          <td class="day-from">{{ d.from }}</td>
          <td class="day-to">{{ d.to }}</td>
          <td>{{ d.distance || "" }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import Models from '@/models';
import moment from 'moment';

export default {
  name: "tour-page-index",
  data () {
    return {
      loading: true,
      tour: {
        name: "",
        days: []
      }
    }
  },
  created() {
    this.loading = true;
    Models.loadTour(this.$parent.name)
      .then(tour => {
        this.tour = tour;
        this.loading = false;
      });
  },
  filters: {
    formatDate(s) {
      return s ? moment(s).format("DD/MM/YYYY") : "";
    }
  }
}
</script>

<style scoped>
</style>
