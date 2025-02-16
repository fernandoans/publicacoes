program meu_menu;
var
  opc: integer;
begin
  repeat
    writeln('------ MENU ------');
    writeln('1. Cadastro');
    writeln('2. Busca');
    writeln('3. Ajuda');
    writeln('4. Sair');

    write('Sua opção é: ');
    read(opc);

    if (opc = 1) then
      writeln('Opção cadastro....')
    else if (opc = 2) then
      writeln('Opção busca....')
    else if (opc = 3) then
      writeln('Mostrar ajuda....')
    else if (opc = 4) then
      writeln('Até a próxima....')
    else
      writeln('Opção inválida!');
   
  until (opc = 4);
end.