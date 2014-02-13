var util = require('util');
var connect = require('connect');
var port = 1337;

connect.createServer(connect.static(__dirname)).listen(port);
util.puts('Start on http://localhost:' + port);
util.puts('Press Ctrl + C to stop.');
