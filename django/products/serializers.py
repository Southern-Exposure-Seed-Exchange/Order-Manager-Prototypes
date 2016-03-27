"""Serializers for Products & Associated Models."""
from rest_framework import serializers

from .models import Category, Product, ProductVariant


class CategorySerializer(serializers.HyperlinkedModelSerializer):
    class Meta(object):
        model = Category
        fields = ('name', 'description', 'parent')


class ProductSerializer(serializers.HyperlinkedModelSerializer):
    class Meta(object):
        model = Product
        fields = ('name', 'description', 'category', 'is_organic',
                  'is_heirloom', 'is_south_east', 'is_active')


class ProductVariantSerializer(serializers.HyperlinkedModelSerializer):
    class Meta(object):
        model = ProductVariant
        fields = ('sku', 'product', 'weight', 'price', 'quantity')
