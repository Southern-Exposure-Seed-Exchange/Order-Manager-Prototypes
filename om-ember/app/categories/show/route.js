import Ember from 'ember';

export default Ember.Route.extend({
  actions: {
    delete(category) {
      if (confirm("Are you sure you want to delete this record? This is irreversible!")) {
        category.destroyRecord().then(() => {
          this.transitionTo('categories');
        });
      }
      return false;
    },
  },
});
