import CategoriesBaseController from '../base/controller';

export default CategoriesBaseController.extend({
  actions: {
    cancel() {
      this.get('model').rollbackAttributes();
      this.transitionToRoute('categories');
    },
  },
});
