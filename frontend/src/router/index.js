import Vue from 'vue'
import Router from 'vue-router'
import TourMain from '@/components/TourMain'
import TourList from '@/components/TourList'
import TourPage from '@/components/TourPage'
import TourDay from '@/components/TourDay'

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
            name: "TourPage",
            props: true,
            component: TourPage
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
