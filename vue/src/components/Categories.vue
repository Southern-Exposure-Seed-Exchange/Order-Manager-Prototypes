<template>
<h1>Categories</h1>

<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Parent</th>
      <th>Categories</th>
      <th>Products</th>
    </tr>
  </thead>
  <tbody>
    <tr v-for="category in categories"> 
      <td>{{ category.name }}</td>
      <td>{{ category.parent.name }}</td>
      <td>{{ category.children.length }}</td>
      <td>{{ category.products.length }}</td>
    </tr>
  </tbody>
</table>
</template>

<script>
export default {
  data () {
    return {
      categories: []
    }
  }
}
</script>
