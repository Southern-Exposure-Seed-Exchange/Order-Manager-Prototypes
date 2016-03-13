import DS from 'ember-data';

export default DS.Model.extend({
  name: DS.attr('string'),
  description: DS.attr('string'),
  parent: DS.belongsTo('category', { inverse: 'children'}),
  children: DS.hasMany('category'),
  products: DS.hasMany('product'),
});
