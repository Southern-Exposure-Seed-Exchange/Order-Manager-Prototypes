import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.store.createRecord('product', {name: "", description: "", isActive: true});
  }
});
