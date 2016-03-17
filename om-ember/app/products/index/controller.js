import Ember from 'ember';

export default Ember.Controller.extend({
  createdProducts: Ember.computed.filterBy('model', 'isNew', false),
  globalVariantToggle: false,
  actions: {
    toggleSkus() {
      this.set('globalVariantToggle', !this.get('globalVariantToggle'));
    },
  },
});
