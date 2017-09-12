import axios from 'axios';

export default {
  loadIndex() {
    return axios.get("/static/data/index.json")
      .then(res => res.data);
  },

  loadTour(name) {
    return axios.get(`/static/data/tours/${name}.json`)
      .then(res => res.data);
  },

  loadDailyTrack(date) {
    return axios.get(`/static/data/daily/${date}.json`)
      .then(res => res.data);
  },

  loadTourTrack(name) {
    return axios.get(`/static/data/tracks/${name}.json`)
      .then(res => res.data);
  }
};
