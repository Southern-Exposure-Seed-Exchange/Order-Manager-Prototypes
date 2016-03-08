import DS from 'ember-data';
import Ember from 'ember';

export default DS.RESTAdapter.extend({
  // Spock
  //host: 'http://localhost:8081',
  headers: {
    "Content-Type": 'application/json',
    "Accept": "application/json",
    "X-XSRF-TOKEN": Ember.get(document.cookie.match(/XSRF\-TOKEN\=([^;]*)/), "1"),
  }
});
