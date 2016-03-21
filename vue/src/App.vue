<template>
  <div id="app">
    <div id='header'>
      <div id='left-header'></div>
      <div id='right-header'></div>
    </div>
    <nav></nav>
    <router-view></router-view>
  </div>
</template>

<script>
import Nav from './components/Nav'

export default {
  components: {
    Nav
  }
}
</script>

<style>
* {
  margin: 0;
  padding: 0;
}
html {
  height: 100%;
}

#app {
  height: 100%;
}

</style>
