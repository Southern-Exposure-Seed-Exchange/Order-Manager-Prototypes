import DS from 'ember-data';

export default DS.Model.extend({
  product: DS.belongsTo('product', { async: true }),
  sku: DS.attr('string'),
  weight: DS.attr('number'),
  price: DS.attr('number')
});
