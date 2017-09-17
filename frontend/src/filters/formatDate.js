import moment from 'moment';

export default function formatDate(s) {
  return s ? moment(s).format("DD/MM/YYYY") : "";
}
