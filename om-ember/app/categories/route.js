import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    this.store.findAll('category');
    return this.store.query('category', {filter: {parent: null}});
  },
});
