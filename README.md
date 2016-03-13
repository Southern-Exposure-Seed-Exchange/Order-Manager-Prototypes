# SESE Order Manager Prototypes

This repository contains very rough prototypes of various web frontends and
backends for an ERP/Order Manager. Right now just Ember.js, Spock & Yesod
exist, but Servant, Django Rest Framework, Angular prototypes, and possible
others are planned.

## Backends

```
sudo pacman -S ghc alex happy cabal-install
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
cabal sandbox init
cabal install --only-dependencies
cabal build && dist/build/om-spock/op-spock
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
