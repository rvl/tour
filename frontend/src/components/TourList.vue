<template>
  <div class="tour-list">
    <h1>All the tours</h1>

    <p v-if="loading" class="loading">
      Loading...
    </p>
    <table v-else class="tour-list">
      <tbody>
        <tr v-for="(tour, name) in tours">
          <td>
            <router-link :to="{ name: 'TourPage', params: { name } }" :title="tour.description">
              {{ tour.name }}
            </router-link>
          </td>
          <td>
            <span class="tour-countries" v-if="tour.countries && tour.countries.length">
              <span class="flag-icon" :class="`flag-icon-${iso2}`" v-for="iso2 in tour.countries"></span>
            </span>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import Models from '@/models';

export default {
  name: "tour-list",
  data() {
    return {
      tours: {},
      loading: false
    }
  },
  created() {
    this.loading = true;
    Models.loadIndex().then(tours => {
      this.tours = tours;
      this.loading = false;
    });
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
@import "~flag-icon-css/css/flag-icon.css";

.tour-countries {
  margin-left: 1em;
}

.flag-icon {
  margin-left: 0.5em;
}

</style>
