import ProductsBaseController from '../base/controller';

export default ProductsBaseController.extend({
  actions: {
    cancel() {
      this.get('model').rollbackAttributes();
      this.set('errorMessage', '');
      this.transitionToRoute('products');
    },
  },
});
