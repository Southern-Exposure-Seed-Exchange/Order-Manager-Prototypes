<template>
  <div id="app">
    <div id='header'>
      <div id='left-header'>OM</div>
      <div id='right-header'>Welcome, USER_NAME | SETTINGS | SIGN_OUT</div>
    </div>
    <div id='content'>
      <navigation></navigation>
      <div id='main-content'>
        <router-view></router-view>
      </div>
    </div>
  </div>
</template>

<script>
import Navigation from './components/Navigation'

export default {
  components: {
    Navigation
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
  body {
    height: 100%;
    font-size: 1.05em;
  }
  #app {
    height: 100%;
  }

  #header {
    color: white;
    background-color: darkblue;
    padding: 3px 2px;
  }
  #left-header {
    width: 20%;
    display: inline-block;
    text-align: center;
  }
  #right-header {
    width: 78%;
    display: inline-block;
    text-align: right;
  }

  #content {
    height: 100%;
  }
  #content:after {
    clear: both;
  }

  #content > #main-content {
    width: 77%;
    display: inline-block;
    height: 100%;
    margin-top: 1em;
    margin-left: 1em;
  }
</style>
