import axios from 'axios';
import _ from 'lodash';

const blogUrl = process.env.NODE_ENV === "production" ?
      "https://rodney.id.au" : "";

const getData = path => axios.get(`/static/data/${path}`).then(res => res.data);

export default {
  info: {
    index: () => getData("index.json"),
    tour: _.memoize(name => getData(`tours/${name}.json`)),
    blog(tourDay) {
      const url = `${blogUrl}/posts/${tourDay.date}-tour-${tourDay.num}/embed.html`;
      return axios.get(url).then(res => res.data);
    }
  },
  track: {
    all:   ()   => getData(`all-tracks.json`),
    tour:  name => getData(`tracks/${name}.json`),
    daily: date => getData(`daily/${date}.json`)
  }
};
