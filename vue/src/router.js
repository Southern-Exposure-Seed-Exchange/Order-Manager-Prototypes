import Vue from 'vue'
import VueRouter from 'vue-router'

import App from './App'
import Hello from './components/Hello'

Vue.use(VueRouter)
var router = new VueRouter()

router.map({
  '/hello': { component: Hello }
})

router.start(App, '#app')
