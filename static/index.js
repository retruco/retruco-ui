
// pull in desired CSS/SASS files
// Note: index.scss is already imported by bootstrap-loader (see .bootstraprc).
// require('./css/index.scss');

// inject bundled Elm app into div#main

var Elm = require('../src/Main');
Elm.Main.embed(document.getElementById('main'));
