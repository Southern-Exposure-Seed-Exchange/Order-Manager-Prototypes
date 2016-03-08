import DS from 'ember-data';

export default DS.Model.extend({
  name: DS.attr('string'),
  description: DS.attr('string'),
  category: DS.belongsTo('category'),
  isOrganic: DS.attr('boolean'),
  isHeirloom: DS.attr('boolean'),
  isSouthEast: DS.attr('boolean'),

});
