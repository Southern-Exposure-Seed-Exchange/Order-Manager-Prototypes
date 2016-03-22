<template>
<ul id='menu'>
  <li id='tagline'>The Enlightened Order Manager</li>
  <li v-for="page in pages">
    <a v-link="page.path">{{ page.name }}</a>
  </li>
</ul>
</template>

<script>
export default {
  data () {
    return {
      pages: [
        { path: '/dashboard', name: 'Dashboard' },
        { path: '/categories', name: 'Categories' },
        { path: '/xyz', name: 'Products' },
        { path: '/ayz', name: 'Inventory' },
        { path: '/abc', name: 'Customers' },
        { path: '/abz', name: 'Orders' }
      ]
    }
  }
}
</script>

<style scoped lang="sass">
#tagline
  font-size: 0.9em
  text-align: center
  color: lightgray
  padding: 15px 0

ul#menu
  height: 100%
  list-style: none

ul#menu li a
  color: white
  display: block
  padding: 15px 20px
  text-decoration: none

ul#menu li a.v-link-active
  color: black
  background-color: darkgray

ul#menu li a:hover
  color: black
  background-color: lightgray
</style>
