const WebSocket = require('ws');
 
const wss = new WebSocket.Server({ port: 8080 });
 
wss.on('connection', function connection(ws) {
    console.log('hello dare')
    ws.on('message', function incoming(message) {
        console.log(message)
        ws.send(message)
    });
 
});