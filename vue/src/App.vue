<template>
  <div id='header'>
    <div id='left-header'>OM</div>
    <div id='right-header'>Welcome, USER_NAME | SETTINGS | SIGN_OUT</div>
  </div>
  <div id='content'>
    <nav id='nav'><navigation></navigation></nav>
    <div id='main-content'>
      <router-view></router-view>
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

<style lang="sass">
  @import "~bourbon/app/assets/stylesheets/bourbon";
  @import "~bourbon-neat/app/assets/stylesheets/neat";
  *
    margin: 0
    padding: 0
  html, #app, body
    height: 100%
  body
    font-size: 1.03em

  $left_columns: 2.5
  $right_columns: 12 - $left_columns

  #header
    height: 1.75em
    padding: 5px
    color: white
    background-color: #494990
    +row()
  #left-header
    text-align: center
    +span-columns($left_columns)
  #right-header
    text-align: right
    +span-columns($right_columns)

  #content
    height: 100%
    +row()
  #nav
    height: 100%
    background-color: #4D4D4D
    +span-columns($left_columns)
  #main-content
    height: 100%
    padding-top: 0.8em
    padding-right: 0.8em
    +span-columns($right_columns)
</style>
