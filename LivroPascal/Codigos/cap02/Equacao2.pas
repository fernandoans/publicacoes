program bhaskara;

{ Função: Um trecho de código que retorna um valor      }
{ Procedimento: Um trecho de código que NÃO retorna valor }

uses math;  { Para usar a função sqrt() }

{ Função para ler um número inteiro com uma mensagem personalizada }
function lerNumero(msg: string): integer;
begin
  write(msg);
  readln(lerNumero);
end;

{ Função para calcular o Delta }
function calcularDelta(a, b, c: integer): real;
begin
  calcularDelta := (b*b) - (4*a*c);
end;

{ Procedimento para calcular e exibir as raízes, se existirem }
procedure calcularRaizes(a, b: integer; delta: real);
var 
  raiz1, raiz2: real;
begin
  if delta < 0 then
    writeln('Não existem raízes reais.')
  else if delta = 0 then
    writeln('Existe apenas uma raiz real: ', -b / (2 * a) :5:2)
  else
    begin
      raiz1 := (-b + sqrt(delta)) / (2*a);
      raiz2 := (-b - sqrt(delta)) / (2*a);
      writeln('Delta: ', delta:5:2);
      writeln('x´ : ', raiz1:5:2);
      writeln('x´´ : ', raiz2:5:2);
    end;  
end;


{ Procedimento para ler os coeficientes da equação }
procedure lerCoeficientes(var a, b, c: integer);
begin
  a := lerNumero('Informe o valor de a:'); 
  b := lerNumero('Informe o valor de b:'); 
  c := lerNumero('Informe o valor de c:'); 
end;

{ Programa Principal }
var
  a, b, c: integer;
  delta: real;
begin
  lerCoeficientes(a, b, c);
  delta := calcularDelta(a, b, c);
  writeln('Para a função ', a, 'x² + ', b, 'x + ', c, ' temos:');
  calcularRaizes(a, b, delta);
end.