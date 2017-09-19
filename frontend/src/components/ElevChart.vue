<template>
  <div class="chart-container">
    <canvas ref="canvas" width="640" :height="chartHeight"></canvas>
  </div>
</template>

<script>
import axios from 'axios';
import _ from 'lodash';
import Chart from 'chart.js';

const ceil100 = n => Math.ceil(n / 100) * 100;
const floor100 = n => Math.floor(n / 100) * 100;

export default {
  name: "elev-chart",
  props: ["date"],
  data() {
    return {
      data: null,
      elev: null,
      chartHeight: 200
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
      this.totalDist = _.last(this.elev).x;
    },
    calcHeight() {
      const axisHeight = 30;
      this.maxElev = ceil100(_.maxBy(this.data, "ele").ele);
      this.minElev = floor100(_.minBy(this.data, "ele").ele);
      this.chartHeight = axisHeight + (this.maxElev - this.minElev) * 0.2;
    },
    setTicks() {
      // requires calcHeight to be called previously
      const yAxis = this.chart.options.scales.yAxes[0];
      yAxis.ticks.min = this.minElev;
      yAxis.ticks.max = this.maxElev;

      const xAxis = this.chart.options.scales.xAxes[0];
      xAxis.ticks.max = ceil100(this.totalDist);
    },
    plot() {
      this.chart.data.datasets[0].data = this.elev;
      this.setTicks();
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
      const gridLines = {
        color: "rgba(0, 0, 0, 0.1)"
      };
      const fontColor = "rgba(31, 31, 31, 0.9)";
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
                max: 100,
                fontColor
              },
              gridLines
            }],
            yAxes: [{
              ticks: {
                beginAtZero: false,
                stepSize: 100,
                fontColor
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
