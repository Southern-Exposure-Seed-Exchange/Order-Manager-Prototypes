from django.db import models


class Category(models.Model):
    name = models.CharField(
        max_length=100, unique=True, help_text='The name of the category.')
    description = models.TextField(
        blank=True,
        help_text='A description of the Category for displaying on the '
        'website.')
    parent = models.ForeignKey('self', models.PROTECT, blank=True, null=True)

    class Meta(object):
        ordering = ('-name',)


class Product(models.Model):
    name = models.CharField(
        max_length=100, unique=True, help_text='The name of the Product.')
    description = models.TextField(
        blank=True, help_text='A description of the Product for the website.')
    category = models.ForeignKey(
        Category, models.PROTECT,
        help_text='The Category the Product belongs to.')
    is_organic = models.BooleanField(
        default=False, help_text='Is this Product Certified Organic?')
    is_heirloom = models.BooleanField(
        default=False, help_text='Is this Product an Heirloom?')
    is_south_east = models.BooleanField(
        default=False, help_text='Is this Product suited for the South East?')
    is_active = models.BooleanField(
        default=False, help_text='Is this Product Active?')


class ProductVariant(models.Model):
    sku = models.CharField(
        max_length=20, unique=True, help_text='The SKU of this Variant.')
    product = models.ForeignKey(
        Product, models.PROTECT, help_text='The Product of this Variant.')
    weight = models.PositiveIntegerField(
        help_text='The weight of this Variant in milligrams.')
    price = models.PositiveIntegerField(
        help_text='The price of this Variant in cents.')
    quantity = models.PositiveIntegerField(
        help_text='The available quanitity of this Variant.'
    )
