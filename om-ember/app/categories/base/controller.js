import Ember from 'ember';

export default Ember.Controller.extend({
  categories: Ember.computed({get() { return this.store.peekAll('category'); }}),
  validParents: Ember.computed.filter('categories',
    function(category) {
      return category.get('id') !== this.get('model.id');
    }
  ).property('model.id'),
  actions: {
    save() {
      this.get('model').save().then((category) => {
        this.transitionToRoute('categories.show', category.id);
      });
      return false;
    },
  }
});
