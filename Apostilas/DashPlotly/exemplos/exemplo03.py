import dash
import dash_core_components as dcc
import dash_html_components as html
import random
import pandas as pd
from dash.dependencies import Input, Output

# Geração dos Dados
ano = []
qtdMoto = []
qtdCarro = []
for i in range(2010, 2021):
    ano.append(i)
    qtdMoto.append(random.randint(0, 100))
    qtdCarro.append(random.randint(0, 100))

df = pd.DataFrame({
    "Ano": ano,
    "Carro": qtdCarro,
    "Moto": qtdMoto
})

def generate_table(dataframe, max_rows=11):
    return html.Table([
        html.Thead(
            html.Tr([html.Th(col) for col in dataframe.columns])
        ),
        html.Tbody([
            html.Tr([
                html.Td(dataframe.iloc[i][col]) for col in dataframe.columns
            ]) for i in range(min(len(dataframe), max_rows))
        ])
    ])

# Geração do DashBoard

app = dash.Dash()
app.title = 'DENATRAN'
app.layout = html.Div([
    html.H1('Veículos Brasil'),
    html.Div([
        html.Label('Intervalo Anual'),
        dcc.RangeSlider(
            id='rangeAnual',
            min=min(df.Ano),
            max=max(df.Ano),
            marks={i: 'Ano {}'.format(i) if i == min(df.Ano) else str(i) for i in range(min(df.Ano), max(df.Ano))},
            value=[min(df.Ano), max(df.Ano)],
        ),
        html.Div([
            dcc.Graph(id='principal'),
        ]),
    ]),  # DIV principal
    html.Div([
        html.Div([
            generate_table(df),
        ], className='two columns'),
        html.Div([
            dcc.Graph(
                id='boxMoto',
                figure={
                  'data': [{
                    'name': 'Dispersão',
                    'y': df.Moto,
                    'type': 'box',
                  }],
                  'layout': {
                    'title': 'Motos'
                  }
                }
            ),
        ], className='five columns'),
        html.Div([
            dcc.Graph(
                id='boxCarro',
                figure={
                    'data': [{
                        'name': 'Dispersão',
                        'y': df.Carro,
                        'type': 'box',
                    }],
                    'layout': {
                        'title': 'Carros'
                    }
                }
            ),
        ], className='five columns'),
    ])
])

@app.callback(Output('principal', 'figure'),
    [Input('rangeAnual', 'value'), ]
)
def modifica_principal(rangeAnual):
    return {
        'data': [
            {'x': df.Ano, 'y': df.Moto, 'type': 'line', 'name': 'Motos'},
            {'x': df.Ano, 'y': df.Carro, 'type': 'bar', 'name': 'Carros'},
        ],
        'layout': {
            'title': 'Comparativo entre Carros e Motos',
            'xaxis': {
                'range': [rangeAnual[0], rangeAnual[1]]
            },
        }
    }


if __name__ == '__main__':
    app.run_server(debug=True)
