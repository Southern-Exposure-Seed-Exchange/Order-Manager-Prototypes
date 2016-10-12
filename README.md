# SESE Order Manager Prototypes

This repository contains very rough prototypes of various web frontends and
backends for an ERP/Order Manager. Prototypes for Django Rest Framework,
Ember.js, Elm, Servant, Spock & Yesod exist. A Purescript prototype is planned,
and others may be included.

## Implemented

* CRUD Categories/Products
* Listing Product Variants.

## Todo?

If we want to get more in-depth with these, maybe something like this:

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

### Purescript

```
cd purescript
bower i
npm i
npm run dev
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

## Retrospective

After building multiple prototypes, my favorite combination is a Servant
backend with Elm or Purescript as the frontend. Elm would be way easier than
Purescript for people not familiar with statically typed, functional
programming languages - but Purescript provides additional ways of abstracting
and reducing boilerplate code. But both are so much better than classical JS
frameworks that either seem worth it. Servant/Haskell gives me a lot more
confidence in the backend code, and it's easier to understand the API
architecture versus Django. The same applies to Elm/Purescript, you can look at
the top-level calls and follow the messages/views down to the actual code,
while Ember feels more like a scavengar hunt.

The purescript-pux library is basically The Elm Architecture in Purescript. In
both, there's some boiler plate of hooking up nested mesages, updates and views
but it feels much more organized & learnable. It's like learning one pattern
and then repeating it at different scales.

Ember and Django hook up very well together, since both have JSON-API support.
With other options, we'd have to hookup our own api schema, error handling, and
pagination.

### Ember

**Pros**
* Easy to learn
* Plain JS
* Lots of libraries, large community

**Cons**
* Hard to debug
* Slow
* Hard to determine best practices or performance improvements
* Hard to reason about

### Elm

**Pros**
* Statically typed, functional
* Obvious/enforced best practices(The Elm Architecture) - less freedom to do
  whatever you want but easier to understand & write
* Elm Format automatically makes code super legible & a standard style
* Performance improvements are easy & don't affect program architecture
* Semantic Versioning of dependencies keeps upgrades easy & builds from breaking
* Easy integration w/ Websockets
* Fast
* Awesome Compiler Messages
* Much more maintainable, refactoring is easy & guided by compiler

**Cons**
* Small but growing community
* Interacting w/ JS is a bigger process but safer.
* Possible breaking changes in future
* Lack of typeclasses adds some boilerplate code
* Most language development by single person.
* Might have to write some 3rd-party integrations ourself(e.g., if we want to show stripe payments)

### Purescript

**Pros**
* Statically typed, functional
* Can replicate Elm Architecture, but is not limited to it
* Syntax almost identical to Haskell, tougher than Elm for webdevs but easy for Haskellers
* Has typeclasses and more powerful abstractions than Elm
* Language development by group larger than Elm's
* Much more maintainable, refactoring is easy & guided by compiler

**Cons**
* Small community
* Somewhat cryptic compiler messages
* Easy interactions with JS
* Dependency versioning is not great, bower caused some pain points
* Possible breaking changes in future
* Might have to write some 3rd-party integrations ourself(e.g., if we want to show stripe payments)

### Django

**Pros**
* Easy to learn, easy to write
* Lots of libraries, easy to interact w/ 3rd party services
* Lots of Django users
* Django-Rest-Framework has cool features like a web frontend with a built in
  API client.

**Cons**
* Requires extra work for websockets(Django Channels)
* Slow performance
* Needs lots and lots of tests for program verification
* Harder to completely trust refactorings without many tests
* No type checking or compile-time guarantees - relies more on documentation & programmer responsibility

### Servant

**Pros**
* Statically typed, functional
* Way faster than Django(~6x)
* Type system/classes allow easy, uncomplicated abstractions
* Can automatically generate API clients
* Compiler is better than Purescript but not as awesome as Elm's
* Much more maintainable, refactoring is easy & guided by compiler
* Library to allow subscriptions to resource updates via websockets
* Can integrate w/ Swagger, but not as simple as Django-Rest-Framework
* Integrated & Type-Checked API Docs - invalid API docs won't compile!

**Cons**
* Not as many libraries as Django/Python, may have to write a couple ourselves
* Smaller community
* Harder to learn
