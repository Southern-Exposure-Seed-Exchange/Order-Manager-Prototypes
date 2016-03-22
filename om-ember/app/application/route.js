import Ember from 'ember';

export default Ember.Route.extend({
  actions: {
    deleteAndGo(record, route) {
      if (confirm("Are you sure you want to delete this record? This is irreversible!")) {
        record.destroyRecord().then(() => {
          this.transitionTo(route);
        });
      }
      return false;
    }
  }
});
