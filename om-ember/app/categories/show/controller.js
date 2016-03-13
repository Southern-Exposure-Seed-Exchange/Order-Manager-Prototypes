import Ember from 'ember';

export default Ember.Controller.extend({
  childrenSorting: ['name:asc'],
  sortedChildren: Ember.computed.sort('model.children', 'childrenSorting'),
});
