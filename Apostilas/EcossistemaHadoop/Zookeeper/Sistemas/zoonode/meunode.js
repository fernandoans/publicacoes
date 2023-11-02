var zookeeper = require('node-zookeeper-client');
var client = zookeeper.createClient('localhost:2181,localhost:2182,localhost:2183');
var path = process.argv[2];
console.log('Estado Atual: %s', client.getState());
var sessionTimeout = client.getSessionTimeout();
console.log('Tempo da Sessão: %s', sessionTimeout)

client.once('connected', function () {
 console.log('Conectado ao servidor.');
 console.log('Estado Atual: %s', client.getState());
 var id = client.getSessionId();
 console.log('Identificação: %s', id.toString('hex'));

 client.create(path, function (error) {
   if (error) {
     console.log('Falhou para criar o node: %s para: %s.', path, error);
   } else {
     console.log('Node: %s is criado corretamente.', path);
   }
   client.close();
 });
});
client.connect();
