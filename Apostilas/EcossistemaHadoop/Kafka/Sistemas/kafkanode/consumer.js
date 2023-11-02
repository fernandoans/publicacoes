const { Kafka } = require("kafkajs");

run();

async function run() {
    try {
        const kafka = new Kafka({
            "clientId": "myapp",
            "brokers": ["localhost:9092"]
        });
        const consumer = kafka.consumer({ "groupId": "teste" });
        console.log("Conectando...");
        await consumer.connect();
        console.log("Consumidor Conectado!");
        consumer.subscribe({
            "topic": "Usuarios",
            "fromBeginning": true
        })

        await consumer.run({
            "eachMessage": async result => {
                console.log(`Mensagem recebida ${result.message.value} da partição ${result.partition}`);
            }
        });
        console.log("Consumidor Rodando...");
    } catch (ex) {
        console.error(`Alguma coisa aconteceu ${ex}`)
    }
}
