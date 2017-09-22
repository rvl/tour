import TourMap from "./components/TourMap.vue";
import ElevChart from "./components/ElevChart.vue";

export default {
  install(Vue, options) {
    Vue.component('tour-map', TourMap);
    Vue.component('tour-elev-chart', ElevChart);
  }
};
