import Vue from 'vue'
import VueRouter from 'vue-router'

import App from './App'
import Categories from './components/Categories'

/* eslint-disable no-new */
Vue.use(VueRouter)
var router = new VueRouter()

router.map({
  '/categories': { component: Categories }
})

router.start(App, 'app')
