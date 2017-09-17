import axios from 'axios';

const blogUrl = process.env.NODE_ENV === "production" ?
      "https://rodney.id.au" : "";

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
  },

  loadAllTrack() {
    return axios.get(`/static/data/all-tracks.json`)
      .then(res => res.data);
  },

  loadBlogHtml(tourDay) {
    return axios.get(`${blogUrl}/posts/${tourDay.date}-tour-${tourDay.num}/embed.html`)
      .then(res => res.data);
  }
};
