import Ember from 'ember';

export default Ember.Controller.extend({
  categories: Ember.computed({get() { return this.store.findAll('category'); }}),
  validCategories: Ember.computed.filterBy('categories', 'isNew', false),

  isValid: Ember.computed.and('categoryIsValid'),
  categoryIsValid: Ember.computed('model.category', function() {
    return this.get('model.category.id');
  }),
  actions: {
    save() {
      let isValid = this.get('isValid');
      console.log(isValid);
      if (isValid) {
        this.set('errorMessage', '');
        this.get('model').save().then(() => {
          this.transitionToRoute('products');
        });
      } else {
        if (!this.get('categoryIsValid')) {
          this.set('model.errors.category', ['A Category is required.']);
        }
      }
      return false;
    },
  }
});
