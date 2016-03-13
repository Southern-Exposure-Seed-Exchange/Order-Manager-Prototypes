import Ember from 'ember';

export default Ember.Controller.extend({
  roots: Ember.computed.filter('model.@each.parent',
    function (category) {
      return category.get('parent.id') == null && !category.get('isNew');
  }),
});
