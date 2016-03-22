import ProductBaseController from '../base/controller';

export default ProductBaseController.extend({
    cancel() {
      this.get('model').rollbackAttributes();
      this.transitionToRoute('products.show', this.get('model'));
    },
});
