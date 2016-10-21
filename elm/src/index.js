'use strict';

require('./index.html');
require('./styles/styles.sass');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var app = Elm.Main.embed(mountNode);

// Elm port to set page title
app.ports.setTitle.subscribe(function(title) {
  document.title = title + " - OM Order Manager";
});


// Elm ports to call `window.confirm()` & return the result.
var confirmPorts = {
  'confirmCategoryDeletion': 'categoryDeleteWasConfirmed',
  'confirmProductDeletion': 'productDeleteWasConfirmed',
}
for (var subName in confirmPorts){
  var sendName = confirmPorts[subName];
  app.ports[subName].subscribe(function(data) {
    var id = data[0];
    var text = data[1];
    var wasConfirmed = window.confirm(text);
    if (wasConfirmed) {
      app.ports[sendName].send(id);
    }
  });
}
