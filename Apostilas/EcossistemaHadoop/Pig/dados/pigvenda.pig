
salesTable = LOAD 'SalesJan2009.csv' USING PigStorage(',') AS (Transaction_date:chararray, Product:chararray, Price:chararray, Payment_Type:chararray, Name:chararray, City:chararray, State:chararray, Country:chararray, Account_Created:chararray, Last_Login:chararray, Latitude:chararray, Longitude:chararray);
GroupByCountry = GROUP salesTable BY Country;
CountByCountry = FOREACH GroupByCountry GENERATE CONCAT((chararray)$0,CONCAT(':',(chararray)COUNT($1)));
STORE CountByCountry INTO 'pig_saida2_vendas' USING PigStorage('\t');
