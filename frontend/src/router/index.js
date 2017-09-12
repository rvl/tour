import Vue from 'vue'
import Router from 'vue-router'
import TourList from '@/components/TourList'
import TourPage from '@/components/TourPage'
import TourPageIndex from '@/components/TourPageIndex'
import TourDay from '@/components/TourDay'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: "TourList",
      props: true,
      component: TourList
    },
    {
      path: "/:name",
      props: true,
      component: TourPage,
      children: [
        {
          path: "",
          name: "TourPage",
          component: TourPageIndex
        },
        {
          path: ":date",
          name: "TourDay",
          props: true,
          component: TourDay
        }
      ]
    }
  ]
})
