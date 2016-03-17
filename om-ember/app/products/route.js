import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.store.findAll('product');
  },
  setupController(model, controller) {
    this._super(model, controller);
    controller.set(
      'createdProducts', Ember.computed.filterBy('model', 'isNew', false));
  },
});
