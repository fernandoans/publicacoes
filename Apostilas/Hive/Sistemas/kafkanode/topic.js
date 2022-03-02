const { Kafka } = require("kafkajs");

run();

async function run() {
    try {
        const kafka = new Kafka({
            "clientId": "myapp",
            "brokers": ["192.168.15.5:9092"]
        });
        const admin = kafka.admin();
        console.log("Conectando...");
        await admin.connect();
        console.log("Conectado!");
        await admin.createTopics({
            "topics": [{
                "topic": "Usuarios",
                "numPartitions": 2
            }]
        });
        console.log("Sucesso");
        await admin.disconnect();
        console.log("Desconectado!");
    } catch (ex) {
        console.error(`Alguma coisa aconteceu ${ex}`)
    } finally {
        process.exit(0);
    }
}