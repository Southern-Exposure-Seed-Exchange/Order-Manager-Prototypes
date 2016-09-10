# SESE Order Manager Prototypes

This repository contains very rough prototypes of various web frontends and
backends for an ERP/Order Manager. Right now just Ember.js, Servant, Spock &
Yesod exist, but Django Rest Framework, & Angular prototypes, and possible
others are planned.

## Implemented

* CRUD Categories/Products
* Listing Product Variants.

## Todo?

Something like this:

* Create/Update/Delete/View Products/Variants
* CRU Customers
* CR Inventory
* CR Orders

## Backends

```
sudo pacman -S ghc alex happy cabal-install stack
```

### Django

```
cd django
pip install -r requirements
createdb om-django
./manage.py migrate
./manage.py runserver 0.0.0.0:3000
```

### Servant

```
cd servant
createdb om-servant
make
```

### Spock

```
cd spock
make
```

### Yesod

```
cabal install yesod-bin

cd yesod
cabal sandbox init
cabal install --only-dependencies
yesod devel
```


## Frontends

```
sudo pacman -S npm bower
```

### Elm

```
cd elm
npm i -g elm
npm i
npm run dev
```

### Ember

```
cd ember
npm install
bower install
ember serve --proxy 'http://localhost:3000'
```

### Vue

```
cd vue
npm install
npm run dev
```


## Performance

### Backends

#### Django

* Cat/Prod/Variants import takes ~2 seconds.

```
$ siege http://localhost:3000/products/ -c 20 -t 60s -b

Lifting the server siege...
Transactions:		         378 hits
Availability:		      100.00 %
Elapsed time:		       59.09 secs
Data transferred:	      176.84 MB
Response time:		        3.05 secs
Transaction rate:	        6.40 trans/sec
Throughput:		        2.99 MB/sec
Concurrency:		       19.54
Successful transactions:         378
Failed transactions:	           0
Longest transaction:	        5.40
Shortest transaction:	        1.25
```

#### Servant

* Cat/Prod/Variants import takes 0.6 seconds

```
$ siege http://localhost:3000/products/ -c 20 -t 60s -b

Lifting the server siege...
Transactions:		        1082 hits
Availability:		      100.00 %
Elapsed time:		       59.64 secs
Data transferred:	      625.14 MB
Response time:		        1.09 secs
Transaction rate:	       18.14 trans/sec
Throughput:		       10.48 MB/sec
Concurrency:		       19.74
Successful transactions:        1082
Failed transactions:	           0
Longest transaction:	        4.07
Shortest transaction:	        0.36
```

### Frontends

#### Ember

* Displaying the Products Page with a table of 1441 Product rows takes 9.8s of
  scripting time & 0.5s of rendering time.
* Un-hiding 1855 Variants on the Products Page takes 2.9s of scripting time &
  0.4s of rendering time.
* Hiding 1855 Variants on the Products Page takes 1.1s of scripting time & 0.1s
  of rendering time.
* Loading directly into a Product Details Page takes 1.7s of scripting time.

#### Elm

* Displaying the Products Page with a table of 1441 Product rows takes 0.5s of
  scripting time & 0.1s of rendering time.
* Un-hiding 1855 Variants on the Products Page takes 0.4s of scripting time &
  0.3s of rendering time.
* Hiding 1855 Variants on the Products Page takes 0.3s of scripting time & 0.2s
  of rendering time.
* Loading directly into a Product Details Page takes 0.1s of scripting time.
