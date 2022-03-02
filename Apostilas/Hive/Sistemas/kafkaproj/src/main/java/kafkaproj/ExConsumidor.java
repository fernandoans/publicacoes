package kafkaproj;

import java.time.Duration;
import java.util.Collections;
import java.util.Properties;

import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;

import kafka.utils.ShutdownableThread;

/**
* Kafka Consumer with Example Java Application
*/
public class ExConsumidor extends ShutdownableThread {
    private final KafkaConsumer<Integer, String> consumer;
    private final String topic;
     
    public static final String KAFKA_SERVER_URL = "192.168.15.5";
    public static final int KAFKA_SERVER_PORT = 9092;
    public static final String CLIENT_ID = "SimplesConsumidor";
 
    public ExConsumidor(String topic) {
        super("KafkaConsumidor", false);
        Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, KAFKA_SERVER_URL + ":" + KAFKA_SERVER_PORT);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, CLIENT_ID);
        props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true");
        props.put(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "1000");
        props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "30000");
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.IntegerDeserializer");
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer");
 
        consumer = new KafkaConsumer<>(props);
        this.topic = topic;
    }
 
    @Override
    public void doWork() {
        consumer.subscribe(Collections.singletonList(this.topic));
        ConsumerRecords<Integer, String> records = consumer.poll(Duration.ofMillis(100));
        for (ConsumerRecord<Integer, String> registro : records) {
            System.out.println("Recebendo mensagem: (" + registro.key() + ", " + registro.value() + ") at offset " + registro.offset());
        }
    }
 
    @Override
    public String name() {
        return null;
    }
 
    @Override
    public boolean isInterruptible() {
        return false;
    }
}