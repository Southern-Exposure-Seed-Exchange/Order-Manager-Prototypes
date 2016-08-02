import DS from 'ember-data';
import Ember from 'ember';

// Servant
export default DS.RESTAdapter.extend({
//
//Django
//export default DS.JSONAPIAdapter.extend({
  headers: Ember.computed(() => {
    return {
      // Servant
      "Content-Type": 'application/json',
      "Accept": "application/json",
      // Django
      //"X-XSRF-TOKEN": Ember.get(document.cookie.match(/XSRF\-TOKEN\=([^;]*)/), "1"),
    };
  }).volatile(),
});
