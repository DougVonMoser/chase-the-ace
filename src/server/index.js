const WebSocket = require('ws');
 
const wss = new WebSocket.Server({ port: 8080 });
 
wss.broadcast = function broadcast(data) {
    wss.clients.forEach(function each(client) {
      if (client.readyState === WebSocket.OPEN) {
        client.send(data);
      }
    });
  };

let getUpToSpeed = null;
wss.on('connection', function connection(ws) {
    if (getUpToSpeed){
      ws.send(getUpToSpeed);
    }
    console.log('hello dare')
    ws.on('message', function incoming(message) {
        // getUpToSpeed = message;
        console.log(message)
        // ws.send(message)
        wss.broadcast(message);
    });
 
});


// Broadcast to everyone else.
// wss.on('connection', function connection(ws) {
//     ws.on('message', function incoming(data) {
//       // Broadcast to everyone else.
//       wss.clients.forEach(function each(client) {
//         if (client !== ws && client.readyState === WebSocket.OPEN) {
//           client.send(data);
//         }
//       });
//     });
//   });