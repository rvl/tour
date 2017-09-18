export default function(tourDay) {
  if (tourDay) {
    if (tourDay.from && tourDay.to) {
      return `${tourDay.from} → ${tourDay.to}`;
    } else {
      return tourDay.from;
    }
  }
  return "";
};
