export default {
  // features have a name property which is the track date without dashes
  dateToFeature(date) {
    return date ? date.replace(/-/g, "") : "";
  },

  featureToDate(feature) {
    return feature ? [feature.slice(0, 4), feature.slice(4, 6), feature.slice(6, 8)].join("-") : "";
  }
}
