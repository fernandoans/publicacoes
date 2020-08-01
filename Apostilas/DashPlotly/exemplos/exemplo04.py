import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html
import random

# Geração dos Dados
ano = []
dados = []
for i in range(2010, 2021):
    ano.append(i)
    dados.append(random.randint(10, 500))

# Montagem do Layout
app = dash.Dash()
app.layout = html.Div([
    html.Div([
        html.Div([
            dcc.Dropdown(
                id='cor',
                options=[i for i in [
                    {'label': 'Azul Metal', 'value': '#4682B4'},
                    {'label': 'Salmão', 'value': '#FA8072'},
                    {'label': 'Verde Verão', 'value': '#00FF7F'},
                    {'label': 'Rosa', 'value': '#FF14B3'},
                    {'label': 'Oliva', 'value': '#808000'},
                    {'label': 'Siena', 'value': '#A05220'}
                ]],
                value='#2ECC40'
            )
        ], className="six columns"),
        html.Div([
            dcc.Slider(
                id='tamanho',
                min=5,
                max=50,
                marks={i: 'Tamanho {}'.format(i) if i == 5 else str(i) for i in range(5, 50, 5)},
                value=20
            )
        ], className="six columns"),
    ], className="row"),
    html.Div([
        dcc.Graph(id='central'),
    ]),
])

# Modificações no Gráfico
@app.callback(Output('central', 'figure'), [
    Input('cor', 'value'),
    Input('tamanho', 'value'),
])
def alterar_central(cor, tamanho):
    return {
        'data': [{
            'x': ano,
            'y': dados,
            'type': 'scatter',
            'mode': 'markers',
            'marker': {
                'color': cor,
                'size': tamanho
            }
        }],
        'layout': {
            'xaxis': {'range': [min(ano)-1, max(ano)+1]},
            'yaxis': {'range': [10, 500]},
            'transition': {
                'duration': 500,
                'easing': 'cubic-in-out'
            }
        }
    }

if __name__ == '__main__':
    app.run_server(debug=True)