import Vue from 'vue'
import Router from 'vue-router'
import TourMain from '@/components/TourMain'
import TourList from '@/components/TourList'
import TourPage from '@/components/TourPage'
import TourPageIndex from '@/components/TourPageIndex'
import TourDay from '@/components/TourDay'
import MapAll from '@/components/MapAll'
import MapTour from '@/components/MapTour'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: "/",
      component: TourMain,
      children: [{
        path: "",
        name: "TourList",
        component: TourList
      }, {
        path: ":name",
        props: true,
        component: { props: ["name"], template: `<router-view />` },
        children: [
          {
            path: "",
            name: "TourPageIndex",
            props: true,
            component: TourPageIndex
          },
          {
            path: ":date",
            name: "TourDay",
            props: true,
            component: TourDay
          }
        ]
      }]
    }
  ]
})
