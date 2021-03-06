import Ember from 'ember';
import config from './config/environment';

const Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.route('categories', function() {
    this.route('new');
    this.route('show', {
      path: ':category_id'
    });
    this.route('edit', {
      path: ':category_id/edit'
    });
  });


  this.route('products', function() {
    this.route('new');

    this.route('show', {
      path: ':product_id'
    });

    this.route('edit', {
      path: ':product_id/edit'
    });
  });
});

export default Router;
