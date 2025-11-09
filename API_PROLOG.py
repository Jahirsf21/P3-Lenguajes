import os
from flask import Flask, render_template, request, jsonify
from flask_cors import CORS
from pyswip import Prolog

app = Flask(__name__, template_folder='.')
CORS(app)

prolog = Prolog()

try:
    ruta_proyecto = os.path.abspath(os.path.dirname(__file__))
    archivos_prolog = [
        os.path.join(ruta_proyecto, 'logicadenegocios', 'hechos.pl'),
        os.path.join(ruta_proyecto, 'logicadenegocios', 'reglas.pl'),
        os.path.join(ruta_proyecto, 'logicadenegocios', 'predicados.pl'),
    ]
    for ruta_archivo in archivos_prolog:
        prolog.consult(ruta_archivo) 
except Exception as e:
    print(f"Error al cargar los archivos de Prolog: {e}")

@app.route('/query', methods=['POST'])
def query():
    try:
        data = request.json
        predicado = data.get('predicado')
        parametros = data.get('parametros', [])  
        parametros_str = ','.join(map(str, parametros))
        consulta = f"{predicado}({parametros_str})."
        print(consulta)
        respuesta = list(prolog.query(consulta))
        
        if not respuesta:
            return jsonify({'resultado': False})
        return jsonify({'resultado': True, 'data': respuesta})
    except Exception as e:
        print(f"Error durante la ejecución de la consulta: {e}")
        return jsonify({'error': str(e)}), 400

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')