import Vue from 'vue'
import Categories from 'src/components/Categories'

describe('Categories.vue', () => {
  it('should render correct contents', () => {
    const vm = new Vue({
      template: '<div><categories></categories></div>',
      components: { Categories }
    }).$mount()
    expect(vm.$el.querySelector('h1').textContent).toBe('Categories')
  })
})

// also see example testing a component with mocks at
// https://github.com/vuejs/vue-loader-example/blob/master/test/unit/a.spec.js#L24-L49
