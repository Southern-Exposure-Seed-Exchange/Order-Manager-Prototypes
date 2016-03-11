import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tbody',
  classNameBindings: ['hasVariants:pointer'],

  showVariants: function() {
    return (this.get('globalToggle') || this.get('clickToggle')) &&
           this.get('hasVariants');
  }.property('clickToggle', 'hasVariants', 'globalToggle'),
  hasVariants: function() {
    return this.get('product.productVariants.length') !== 0;
  }.property('product.productVariants.length'),

  clickToggle: false,
  actions: {
    clickProduct() { this.set('clickToggle', !this.get('clickToggle')); },
  },
});
