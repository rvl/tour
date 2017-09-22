<template>
  <div class="tour-day">
    <div class="tour-day-popup" :class="{ collapsed }">
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
        <router-link :to="{ name: 'TourList' }">Tours</router-link>
        &rsaquo;
        <router-link :to="{ name: 'TourPage', params: { name: $parent.name } }">
          <span v-if="tour" v-text="tour.name" />
          <span v-else v-text="$parent.name" />
        </router-link>
        &rsaquo;
        {{ date | formatDate }}

        <template v-if="nextDate || prevDate">
        ●
        <router-link v-if="prevDate" :to="{ params: { date: prevDate } }">&laquo; Prev Day</router-link>
        <template v-if="prevDate && nextDate">|</template>
        <router-link v-if="nextDate" :to="{ params: { date: nextDate } }">Next Day &raquo;</router-link>
        </template>

        <template v-if="tourHtml">
        ●

        <a href="#" @click.prevent="collapsed = !collapsed" class="collapse-button">
          {{ collapsed ? "Read the blog" : "Hide text" }}
        </a>
        </template>
      </p>

      <section class="blog loading" v-if="tourHtml === null">
        Loading...
      </section>
      <section class="blog" v-else v-html="tourHtml" />
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
import formatDate from '@/filters/formatDate';

export default {
  name: "tour-day",
  props: ["date"],
  data () {
    return {
      tour: {
        name: null
      },
      tourDay: {
        from: null,
        to: null,
        url: null,
        dist: null,
        start: null,
        finish: null
      },
      tourHtml: null,
      collapsed: true,
      prevDate: null,
      nextDate: null
    }
  },
  methods: {
    load(date) {
      if (date) {
        Models.info.tour(this.$parent.name).then(tour => {
          this.tour = tour;
          const index = _.findIndex(tour.days, { date });

          if (index >= 0) {
            this.tourDay = tour.days[index];
            this.prevDate = tour.days[index - 1] ? tour.days[index - 1].date: null;
            this.nextDate = tour.days[index + 1] ? tour.days[index + 1].date: null;

            Models.info.blog(this.tourDay).then(html => {
              this.tourHtml = html;
            })
              .catch(err => {
                this.tourHtml = "";
              })
          } else {
            this.tourDay = null;
            this.prevDate = null;
            this.nextDate = null;
          }
        });
      } else {
        this.tourDay = null;
        this.tourHtml = null;
      }
    },
    onVisit(date) {
      this.$router.push({ name: "TourDay", params: { name: this.$parent.name, date } });
    }
  },
  created() {
    this.$watch("date", date => {
      this.load(date);
    }, { immediate: true });
  },
  components: {
    TourMap,
    ElevChart
  },
  filters: {
    formatDate
  }
}
</script>

<style lang="scss">
.tour-day {
  h1 {
    margin: 0px;
  }

  .tour-day-popup {
    position: absolute;
    top: 1em;
    left: 4em;
    right: 4em;
    max-width: 40em;
    max-height: calc(100vh - 120px);
    overflow-y: scroll;
    border: 2px solid black;
    border-radius: 8px;
    box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.4);
    background: rgba(255, 255, 255, 0.8);

    padding: 0.2em 0.5em;
    z-index: 1000;

    &.collapsed {
      max-width: none;
      right: auto;

      section {
        display: none;
      }
    }

    section {
      line-height: 1.4em;
    }
  }

  .chart-bottom {
    position: absolute;
    bottom: 0px;
    left: 0px;
    right: 0px;
    padding: 0.5em 1em 1em 1em;

    background-image: linear-gradient(
      rgba(255,255,255,0),
      rgba(255,255,255,0.6)
    );

    pointer-events: none;
  }

  section.blog {
    .info {
      color: #666;
      margin-bottom: 1em;
    }

    .figure {
      background: black;
      color: #ddd;
      padding: 4px;

      p {
        margin: 0.5em;
      }

      img {
        width: 100%;
        height: auto;
      }
    }
  }
}

</style>
