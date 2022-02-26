package kafkaproj;

public class GeraMensagens {
  public static final String TOPIC = "testTopic";
   
  public static void main(String[] args) {
      boolean isAsync = false;
      ExProdutor producerThread = new ExProdutor(TOPIC, isAsync);
      producerThread.start();
  }
}
