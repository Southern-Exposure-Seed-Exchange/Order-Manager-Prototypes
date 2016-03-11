import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    this.store.findAll('productVariant');
    return this.store.findAll('product');
  },
});
