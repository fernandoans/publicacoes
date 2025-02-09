program bhaskara;
var
  a, b, c: integer;
var
  delta: real;

begin
  write('Informe o valor da raiz a: ');
  read(a);
  write('Informe o valor da raiz b: ');
  read(b);
  write('Informe o valor da raiz c: ');
  read(c);

  delta := (b*b) - (4*a*c);

  writeln('Para a função ', a, 'x² + ', b, 'x + ', c, ' temos:');
  if delta < 0 then
    writeln('Não existem raízes reais.')
  else if delta = 0 then
    writeln('Existe apenas uma raiz real: ', -b / (2 * a) :5:2)
  else
    begin
      writeln('Delta: ', delta:5:2);
      writeln('x´ : ', (-b + sqrt(delta)) / (2*a) :5:2);
      writeln('x´´ : ', (-b - sqrt(delta)) / (2*a) :5:2);
    end;
end.