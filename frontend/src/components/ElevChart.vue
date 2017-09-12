<template>
  <div class="chart-container">
    <canvas ref="canvas" width="640" :height="chartHeight"></canvas>
  </div>
</template>

<script>
import axios from 'axios';
import _ from 'lodash';
import Chart from 'chart.js';

export default {
  name: "elev-chart",
  props: ["date"],
  data() {
    return {
      data: null,
      elev: null,
      chartHeight: 200,
    }
  },
  methods: {
    load() {
      this.data = null;
      this.destroyChart(); // because resizing doesn't work
      axios.get(`/static/data/elev/${this.date}.json`)
        .then(res => {
          this.mungeData(res.data);
          this.calcHeight();
          this.$nextTick(() => {
            // this.chart.resize(); // doesn't work
            this.initChart(); // does work
            this.plot();
          });
        });
    },
    mungeData(data) {
      this.data = data;
      this.elev = _.map(data, d => ({ x: d.dist / 1000, y: d.ele }));
    },
    calcHeight() {
      const axisHeight = 30;
      const ceil100 = n => Math.ceil(n / 100) * 100;
      const floor100 = n => Math.floor(n / 100) * 100;
      this.maxElev = ceil100(_.maxBy(this.data, "ele").ele);
      this.minElev = floor100(_.minBy(this.data, "ele").ele);
      this.chartHeight = axisHeight + (this.maxElev - this.minElev) * 0.2;
    },
    plot() {
      this.chart.data.datasets[0].data = this.elev;

      const yAxis = this.chart.options.scales.yAxes[0];
      yAxis.ticks.min = this.minElev;
      yAxis.ticks.max = this.maxElev;

      this.chart.update();
    },
    destroyChart() {
      if (this.chart) {
        this.chart.destroy();
      }
    },
    initChart() {
      // init chart.js
      const color = Chart.helpers.color;
      this.chart = new Chart(this.$refs.canvas, {
        type: "scatter",
        data: {
          datasets: [{
            label: "Elevation",
            data: [],
            borderColor: "#800000",
            backgroundColor: color("#800000").alpha(0.2).rgbString(),
            borderWidth: 1.0,
            pointRadius: 0,
            lineTension: 0
          }]
        },
        options: {
          maintainAspectRatio: false,
          legend: { display: false },
          scales: {
            xAxes: [{
              ticks: {
                beginAtZero: true,
                min: 0,
                max: 120
              }
            }],
            yAxes: [{
              ticks: {
                beginAtZero: false,
                stepSize: 100
              }
            }]
          }
        }
      });

    }
  },
  beforeDestroy() {
    this.destroyChart();
  },
  created() {
    this.$watch("date", this.load, { immediate: true });
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
canvas {
  width: 100%;
}
</style>
