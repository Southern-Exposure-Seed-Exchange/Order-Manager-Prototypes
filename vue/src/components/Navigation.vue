<template>
<nav>
  <ul id='menu'>
    <li id='tagline'>The Enlightened Order Manager</li>
    <li v-for="page in pages">
      <a v-link="page.path">{{ page.name }}</a>
    </li>
  </ul>
</nav>
</template>

<script>
import Vue from 'vue'
export default Vue.extend({
  data () {
    return {
      pages: [
        { path: '/dashboard', name: 'Dashboard' },
        { path: '/categories', name: 'Categories' }
      ]
    }
  }
})
</script>

<style>
nav {
  width: 20%;
  display: inline-block;
  height: 100%;
  background-color: #4D4D4D;
  float: left
}
#tagline {
  font-size: 0.9em;
  text-align: center;
  color: lightgray;
  padding: 15px 0;
}

ul#menu {
  height: 100%;
}

ul#menu li a {
  color: white;
  display: block;
  padding: 15px 20px;
  text-decoration: none;
}

ul#menu li a.v-link-active {
  background-color: darkgray;
}

ul#menu li a:hover {
  color: black;
  background-color: lightgray;
}

</style>
