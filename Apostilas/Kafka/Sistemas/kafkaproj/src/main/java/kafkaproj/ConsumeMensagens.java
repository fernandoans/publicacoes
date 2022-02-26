package kafkaproj;

public class ConsumeMensagens {
  public static void main(String[] args) {
    ExConsumidor consumerThread = new ExConsumidor("testTopic");
    consumerThread.start();
  }
}
