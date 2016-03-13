import Ember from 'ember';

export default Ember.Controller.extend({
  categories: Ember.computed({get() { return this.store.findAll('category'); }}),
  isValid: Ember.computed('model.name', {
    get() { return !Ember.isEmpty(this.get('model.name')); }
  }),
  actions: {
    save() {
      if (this.get('isValid')) {
        this.set('errorMessage', '');
        this.get('model').save().then((category) => {
          this.transitionToRoute('categories.show', category.id);
        });
      } else {
        this.set('errorMessage', "You must enter a Unique Name.");
      }
      return false;
    },
    updateParent(parent) {
      this.set(this.get('model'), 'parent', parent);
    },
  }
});
