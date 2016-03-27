from decimal import Decimal
import csv
import math

from django.db import transaction

from products.models import Product, ProductVariant, Category


def main():
    with transaction.atomic():
        import_data()

def import_data():
    print("Reading Input File...")
    rows = []
    with open('./OM_Products_Export.csv', newline='') as csv_file:
        reader = csv.DictReader(csv_file)
        rows = [row for row in reader]

    print("Creating Categories...")
    category_names = [row['Category'] for row in rows]
    categories = {}
    for name in set(category_names):
        cat = Category.objects.create(name=name, description="", parent=None)
        categories[name] = cat

    print("Creating Products...")
    products = {}
    for row in rows:
        category = categories[row['Category']]
        name = row['ItemName']
        if name not in products:
            product = Product.objects.create(
                name=name, category=category, description=row['Contents'],
                is_organic=row['Organic'], is_heirloom=row['Heirloom'],
                is_south_east=False, is_active=(not row['DontSell']),
            )
            products[name] = product
    print("Creating Product Variants...")
    for row in rows:
        ProductVariant.objects.create(
            sku=row['LocalSKU'], product=products[row['ItemName']],
            weight=to_centi(row['Weight']),
            price=to_centi(row['Price'].replace('$', '')),
            quantity=(row['QOH'] or 0)
        )

    print("Finished Import")

def to_centi(n):
    return math.floor(Decimal(n or 0) * 100)
