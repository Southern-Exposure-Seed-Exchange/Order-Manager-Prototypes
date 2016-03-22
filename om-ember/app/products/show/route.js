import Ember from 'ember';

export default Ember.Route.extend({
  actions: {
    delete(product) {
      if (confirm("Are you sure you want to delete this record? This is irreversible!")) {
        product.destroyRecord().then(() => {
          this.transitionTo('products');
        });
      }
      return false;
    },
  }
});
