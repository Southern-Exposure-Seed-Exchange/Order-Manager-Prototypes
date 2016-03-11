import Ember from 'ember';

export default Ember.Controller.extend({
  globalVariantToggle: false,
  actions: {
    toggleSkus() {
      this.set('globalVariantToggle', !this.get('globalVariantToggle'));
    },
  },
});
