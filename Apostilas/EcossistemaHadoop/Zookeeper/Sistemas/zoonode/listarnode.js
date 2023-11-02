var zookeeper = require('node-zookeeper-client');
var client = zookeeper.createClient('localhost:2181,localhost:2182,localhost:2183');
var path = process.argv[2];

function listChildren(client, path) {
    client.getChildren(
        path,
        function (event) {
            console.log('Obter o evento watcher: %s', event);
            listChildren(client, path);
        },
        function (error, children, stat) {
            if (error) {
                console.log(
                    'Fallhou para listar o filho de %s para: %s.',
                    path,
                    error
                );
                return;
            }

            console.log('Filhos de %s são: %j.', path, children);
            client.close();
        }
    );
}

client.once('connected', function () {
    console.log('Conectado ao ZooKeeper.');
    listChildren(client, path);
});
client.connect();
