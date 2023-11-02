const { Kafka } = require("kafkajs");
const usuario = process.argv[2];

run();

async function run() {
    try {
        const kafka = new Kafka({
            "clientId": "myapp",
            "brokers": ["localhost:9092"]
        });
        const producer = kafka.producer();
        console.log("Conectando...");
        await producer.connect();
        console.log("Produtor Conectado!");

        // A-M = Part0 ou N-Z = Part1
        const particao = usuario[0] < "N" ? 0 : 1;

        const result = await producer.send({
            "topic": "Usuarios",
            "messages": [
                {
                    "value": usuario,
                    "partition": particao
                }
            ]
        });
        console.log(`Mensagem Enviada: ${JSON.stringify(result)}`);
        await producer.disconnect();
        console.log("Produtor Desconectado!");
    } catch (ex) {
        console.error(`Alguma coisa aconteceu ${ex}`)
    } finally {
        process.exit(0);
    }
}
