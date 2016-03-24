import Ember from 'ember';

/** The form-input component wraps an input or a select element with a label
 * and displaying errors if any exist.
 *
 * This component should be passed a `fieldLabel` and an `errors` list
 * (usually `model.errors.fieldname`).
 *
 * Only a component block should be used, & the content should be the input or
 * select element/component.
 *
 */
export default Ember.Component.extend({
  hasErrors: Ember.computed.notEmpty('errors'),
  errorString: Ember.computed('errors', function() {
    if (!this.get('hasErrors')) { return ''; }
    let errors = this.get('errors').map((e) => { return `<li>${e.message}</li>`; });
    return `<ul>${errors}</ul>`;
  }),
});
