import axios from 'axios';
import _ from 'lodash';
import jQuery from 'jquery';
import configs from '../config';

const config = configs[process.env.NODE_ENV === "production" ? "build" : "dev"]
const staticUrl = `${config.assetsPublicPath}${config.assetsSubDirectory}/`

const getData = path => axios.get(`${staticUrl}data/${path}`).then(res => res.data);

const getBody = html => {
  // now i have two problems
  const bodyExp = /.*<body[^>]*>(.*?)<\/body>/m;
  const match = bodyExp.exec(html.replace(/\n/g, ""));
  return match ? match[1].trim() : "";
};

export default {
  info: {
    index: () => getData("index.json"),
    tour: _.memoize(name => getData(`tours/${name}.json`)),
    blog(tourDay) {
      const page = tourDay.blog || `${tourDay.date}-tour-${tourDay.num}`;
      const postUrl = `${config.blogUrl}/posts/${page}/`;
      return axios.get(`${postUrl}embed.html`).then(res => {
        const data = getBody(res.data);
        const content = jQuery("<div></div>").html(data);
        content.find("img").each((i, el) => {
          const $el = jQuery(el);
          $el.attr("src", postUrl + $el.attr("src"));
        });
        return content[0].innerHTML;
      });
    }
  },
  track: {
    all:   ()   => getData(`all-tracks.json`),
    tour:  name => getData(`tracks/${name}.json`),
    daily: date => getData(`daily/${date}.json`)
  },
  staticUrl
};
