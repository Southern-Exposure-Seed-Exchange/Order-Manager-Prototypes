'use strict';

require('./index.html');
require('./styles/styles.sass');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var app = Elm.Main.embed(mountNode);

app.ports.setTitle.subscribe(function(title) {
  document.title = title + " - OM Order Manager";
})
