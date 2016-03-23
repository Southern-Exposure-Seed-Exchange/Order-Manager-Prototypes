import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tbody',
  classNameBindings: ['hasVariants:pointer'],

  colSpacing: Ember.computed('showCategory', function() {
    return this.get('showCategory') ? '3' : '2';
  }),
  totalWidth: Ember.computed('colSpacing', function() {
    return Number(this.get('colSpacing')) + 3;
  }),

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
