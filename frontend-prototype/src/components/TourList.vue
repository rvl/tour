<template>
  <div class="tour-list">
    <div class="tour-list-popup">
      <h1>All the tours</h1>

      <p v-if="loading" class="loading">
        Loading...
      </p>
      <table v-else>
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
    Models.info.index().then(tours => {
      this.tours = tours;
      this.loading = false;
    });
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style lang="scss" scoped>
@import "~flag-icon-css/css/flag-icon.css";
@import "../styles/settings.scss";

.tour-countries {
  margin-left: 1em;
}

.flag-icon {
  margin-left: 0.5em;
}

h1 {
  margin-top: 0px;
  margin-bottom: 0.2em;
}

.tour-list-popup {
  position: absolute;
  top: 1em;
  right: 4em;
  border: 2px solid black;
  border-radius: 8px;
  box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.4);
  background: rgba(255, 255, 255, 0.8);

  padding: 0.2em 0.5em;
  z-index: 1000;
}

</style>
