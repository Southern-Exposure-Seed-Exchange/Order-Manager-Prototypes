# SESE Order Manager Prototypes

This repository contains very rough prototypes of various web frontends and
backends for an ERP/Order Manager. Right now just Ember.js, Spock & Yesod
exist, but Servant, Django Rest Framework, Angular prototypes, and possible
others are planned.

## Implemented

* CRUD Categories
* Listing Products & Product Variants.

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

### Yesod

```
cabal install yesod-bin

cd yesod
cabal sandbox init
cabal install --only-dependencies
yesod devel
```

### Spock

```
cd spock
make
```


## Frontends

```
sudo pacman -S npm bower
```

### Ember

```
cd ember
npm install
bower install
ember serve --proxy 'http://localhost:3000'
```
