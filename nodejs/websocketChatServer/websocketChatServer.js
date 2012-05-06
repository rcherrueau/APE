/*!
 * \brief   WebSocketChatServer
 *
 * WebSocketChat is a simple test of how to write a chat in full javascript
 * with websocket (for real time responsive application). This test is
 * inspired from Martin Sikora , use Node.js for server side and Twitter
 * Boostrap for style. Because this is a test, the code is not robust.
 *
 * This file is the WebSocket Server implementation in Node.js. This
 * implementation required Worlize/WebSocket-Node (from github). To launch
 * server do:
 \verbatim
  $ npm install websocket
  $ node websocketChatServer # Server listen on 1337
 \endvebatim
 *
 * \see websocketChatClient.html - The UI
 * \see http://martinsikora.com/nodejs-and-websocket-simple-chat-tutorial
 * \see https://github.com/Worlize/WebSocket-Node
 * \see http://nodejs.org/
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    06/05/2012
 */
var WEBSOCKET_PORT = 1337;
var WebSocketServer = require('websocket').server;
var http = require('http');
var url = require('url');
var fs = require('fs');

//! Currently connected clients.
var clients = [];

//! Escaping input strings
function htmlEntities(str) {
  return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;')
    .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

// Server
var server = http.createServer(function(req, res) {
  var route = url.parse(req.url).pathname

  // Ugly url routing ;)
  switch (route) {
  case '/bootstrap.min.css':
    res.writeHeader(200, {"Content-Type": "text/css"});  
    res.end(fs.readFileSync('./bootstrap.min.css','utf8'));  
    break;
  case '/websocketChatClient.html':
  case '/':
  default:
    res.writeHeader(200, {"Content-Type": "text/html"});  
    res.end(fs.readFileSync('./websocketChatClient.html','utf8'));  
    break;
  }
}).listen(WEBSOCKET_PORT);
var wsServer = new WebSocketServer({ httpServer: server });

//! Handler on every time someone tries to connect to the WebSocketServer.
wsServer.on('request', function(request) {
  var connection = request.accept(null, request.origin);

  var index = clients.push(connection) - 1 ;
  var userName = false;

  //! User send a message event.
  connection.on('message', function(message) {
    if (message.type == 'utf8') {
      // Process WebSocket Message.
      if (userName === false) {
        userName = htmlEntities(message.utf8Data);
        userName = htmlEntities(message.utf8Data);
      } else {
        var obj = {
          time:   (new Date()).getTime(),
          text:   htmlEntities(message.utf8Data),
          author: userName,
        };

        // Broadcast message to all connected clients
        var json = JSON.stringify(obj);
        for (var i in clients) {
          clients[i].sendUTF(json);
        }
      }
    }
  });

  //! User diconnect
  connection.on('close', function(connection) {
    // Close user connection.
    if (userName !== false) {
      clients.splice(index, 1);
    }
  });
});

console.log('Server running at http://127.0.0.1:' + WEBSOCKET_PORT);

