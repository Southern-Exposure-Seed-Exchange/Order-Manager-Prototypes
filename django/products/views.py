from rest_framework import viewsets
from .serializers import CategorySerializer, ProductSerializer, ProductVariantSerializer

from .models import Category, Product, ProductVariant


class CategoryViewSet(viewsets.ModelViewSet):
    """API endpoint that allows categories to be viewed or edited."""
    queryset = Category.objects.all()
    serializer_class = CategorySerializer


class ProductViewSet(viewsets.ModelViewSet):
    """API endpoint that allows products to be viewed or edited."""
    queryset = Product.objects.all()
    serializer_class = ProductSerializer


class ProductVariantViewSet(viewsets.ModelViewSet):
    """API endpoint that allows product variants to be viewed or edited."""
    queryset = ProductVariant.objects.all()
    serializer_class = ProductVariantSerializer
