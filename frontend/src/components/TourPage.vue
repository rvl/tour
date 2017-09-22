<template>
  <div class="tour-page-index">
    <div class="tour-page-index-popup">
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
        <tr>
          <th class="date">Date</th>
          <th class="day-from">From</th>
          <th class="day-to">To</th>
          <th class="dist">Dist (km)</th>
        </tr>
      </thead>
      <tbody v-if="loading" class="loading">
        <tr colspan="5"><td>Loading...</td></tr>
      </tbody>

      <tbody v-else>
        <tr v-for="d in tour.days">
          <td class="date">
            <router-link :to="{ name: 'TourDay', params: { date: d.date } }">
              {{ d.date | formatDate }}
            </router-link>
          </td>
          <td class="day-from">{{ d.from }}</td>
          <td class="day-to">{{ d.to }}</td>
          <td class="dist">{{ d.dist || "" }}</td>
        </tr>
      </tbody>
    </table>
  </div>
  </div>
</template>

<script>
import TourMap from '@/components/TourMap';
import Models from '@/models';
import formatDate from '@/filters/formatDate';

export default {
  name: "tour-page-index",
  props: ["name"],
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
    Models.info.tour(this.$parent.name)
      .then(tour => {
        this.tour = tour;
        this.loading = false;
      });
  },
  filters: {
    formatDate
  },
  components: {
    TourMap
  }
}
</script>

<style lang="scss" scoped>
h1 {
  margin-top: 0px;
}

.tour-map-container {
  width: 100%;
  height: 100vh;
}

.tour-page-index-popup {
  position: absolute;
  top: 1em;
  left: 4em;
  max-height: 90vh;
  overflow-y: scroll;
  border: 2px solid black;
  border-radius: 8px;
  box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.4);
  background: rgba(255, 255, 255, 0.8);

  padding: 0.2em 0.5em;
  z-index: 1000;
}

table.tour-index {
  border-collapse: collapse;
  width: 100%;

  thead, tbody  {
    border-bottom: 1px solid #888;
  }

  th, td {
    padding: 0.2em 0.3em;
  }

  td.dist, th.dist {
    text-align: right;
  }

  th.day-from, th.day-to {
    text-align: left;
  }
}

</style>
