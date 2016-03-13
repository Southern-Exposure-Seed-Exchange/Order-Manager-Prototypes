import Ember from 'ember';
import config from './config/environment';

const Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.route('products', function() {
  });

  this.route('categories', function() {
    this.route('show', {
      path: ':category_id'
    });
  });
});

export default Router;
