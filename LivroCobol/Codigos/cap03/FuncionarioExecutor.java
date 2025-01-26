import java.io.*;

public class FuncionarioExecutor {

    private static final String COBOL_PROGRAM = "./Funcionario";

    public static void main(String [] args) {
        new FuncionarioExecutor().executar();
    }

    public void executar() {
        ProcessBuilder processBuilder = new ProcessBuilder(COBOL_PROGRAM);
        processBuilder.directory(new File("."));
        try {
            System.out.println("Iniciando o programa COBOL");
            Process process = processBuilder.start();
            readProcessOutput(process);
            int exitCode = process.waitFor();
            if (exitCode == 0) {
                System.out.println("Programa COBOL finalizado com Sucesso.");
            } else {
                System.out.println("Programa COBOL finalizado com erro: " + exitCode);
            }
        } catch (IOException e) {
            System.out.println("Erro ao iniciar o programa COBOL. " + e.getMessage());
        } catch (InterruptedException e) {
            System.out.println("A execução do programa COBOL foi interrompida " + e.getMessage());
            Thread.currentThread().interrupt();
        }
    }

    private void readProcessOutput(Process process) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String lin;
            System.out.println("Saída do Programa em COBOL:");
            while ((lin = reader.readLine()) != null) {
                System.out.println(lin);
            }
        } catch (IOException e) {
            System.out.println("Erro ao ler a saída do programa COBOL. " + e.getMessage());
        }
    }
}