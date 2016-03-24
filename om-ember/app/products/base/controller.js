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
      if (this.get('isValid')) {
        this.set('errorMessage', '');
        this.set('model.errors.category', '');
        this.get('model').save().then(() => {
          this.transitionToRoute('products.show', this.get('model.id'));
        });
      } else {
        if (!this.get('categoryIsValid')) {
          this.set('model.errors.category', [{message: 'A Category is required.'}]);
        }
      }
      return false;
    },
  }
});
