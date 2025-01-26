program calculadora;
var
  a,b: integer;
begin
  write('Informe o valor de a: ');
  read(a);
  write('Informe o valor de b: ');
  read(b);
  
  writeln('A soma é: ', a+b);
  writeln('A subtração é: ', a-b);
  writeln('A divisão é: ', a/b);
  writeln('A multiplicação é: ', a*b);
end.