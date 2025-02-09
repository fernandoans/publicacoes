program bhaskara;
var
  a, b, c: integer;
var
  delta, tstDelta: real;

begin
  write('Informe o valor da raiz a: ');
  read(a);
  write('Informe o valor da raiz b: ');
  read(b);
  write('Informe o valor da raiz c: ');
  read(c);

  tstDelta := (b*b) - (4*a*c);

  writeln('Para a função ', a, 'x² + ', b, 'x + ', c, ' temos:');
  if tstDelta < 0 then
    writeln('Não existem raízes reais.')
  else if tstDelta = 0 then
    writeln('Raízes iguais a Zero.')
  else
    begin
      delta := sqrt(tstDelta);
      writeln('Delta: ', delta:5:2);
      writeln('x´ : ', (-b + delta) / (2*a) :5:2);
      writeln('x´´ : ', (-b - delta) / (2*a) :5:2);
    end;
end.