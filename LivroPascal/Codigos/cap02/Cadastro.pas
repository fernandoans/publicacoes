program GravarRegistros;

{$mode objfpc}

type
  TCliente = record
    ID: Integer;
    Nome: String[50];
    Idade: Integer;
  end;

var 
  arquivo: Text;
  cliente: TCliente;
begin
  Assign(arquivo, 'clientes.txt');
  Rewrite(arquivo);

  while True do
  begin
    Write('Digite o ID do cliente (ou -1 para sair): ');
    ReadLn(cliente.ID);
    if cliente.ID = -1 then
      break;
    Write('Digite o nome do cliente: ');
    ReadLn(cliente.Nome);
    Write('Digite o idade do cliente: ');
    ReadLn(cliente.Idade);

    WriteLn(arquivo, cliente.ID,';',cliente.Nome,';',cliente.Idade);
  end;
  Close(arquivo);
  WriteLn('Registros gravados com sucesso!');
end.