<script setup>
import { ref } from 'vue'

const query = ref('')
const query2 = ref('')
const pred = ref('')
const result = ref(null)
const loading = ref(false)
const error = ref('')
const campoActivo = ref(null)

async function enviarPredicado(predicado, parametros = []) {
  loading.value = true
  error.value = ''
  result.value = null
  try {
    const response = await fetch('http://127.0.0.1:5000/query', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ predicado, parametros })
    })
    const data = await response.json()
    if (!response.ok) {
      error.value = data.error || 'Error desconocido'
    } else {
      result.value = data
    }
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}

function mostrarCampos(numero, predi) {
  campoActivo.value = numero
  result.value = null
  pred.value = predi
}

async function formarQuery(){
  result.value = null
  const parametros = []
  if (query.value) parametros.push(query.value.toLowerCase())
  if (query2.value) parametros.push(query2.value.toLowerCase())
  if(pred.value === "donde_esta" || pred.value === "ruta") parametros.push("X")
  await enviarPredicado(pred.value, parametros)
  if (result) mostrarResultados(pred.value)
  query.value = null
  query2.value = null
}


function formarString(lista) {
  let resultado = "";
  lista.forEach((element, index) => {
    resultado += element;
    if (index < lista.length - 1) {
      resultado += ", ";
    }
  });
  return resultado;
}
function formarComoGano(lista){
 
  let resultado = "\n"

  lista.forEach((r, i) => {
    const [lugarFinal, objeto, camino] = r;
    resultado += `${i + 1}. Llegar a ${lugarFinal} con ${objeto} e ir por este camino: ${camino.join(" -> ")}\n`;
  });

  return resultado;
}

function formarGane(lista){
  const [camino, inventario, condiciones] = lista;

  let mensaje = "\n";
  mensaje += "Camino: " + camino.join(" -> ") + "\n";
  mensaje += "Inventario: " + inventario.join(", ") + "\n";
  mensaje += "Condiciones cumplidas: " + condiciones.join(", ");

  return mensaje;
}

function mostrarResultados(predi) {
  if (predi === "tomar") {
    if (result.value && result.value.resultado) {
      result.value = "El objeto se ha tomado correctamente"
    } else {
      result.value = "No se pudo tomar el objeto. No está en la ubicación actual o ya fue tomado"
    }
  }
  else if (predi === "usar"){
    if (result.value && result.value.resultado) {
      result.value = "El objeto se ha usado correctamente"
    } else {
      result.value = "No se pudo usar el objeto. No lo tienes o ya fue usado"
    }
  } else if (predi === "puedo_ir"){
    if (result.value && result.value.resultado) {
      result.value = "Sí puedes ir a ese lugar"
    } else {
      result.value = "No puedes ir a ese lugar, no cumples las condiciones requeridas"
    }
  } else if (predi === "mover"){
    if (result.value && result.value.resultado) {
      result.value = "Te has movido a ese lugar"
    } else {
      result.value = "No puedes ir a ese lugar, no cumples las condiciones requeridas"
    }
  } else if (predi === "donde_esta") {
  const lugares = result.value.data[0].X;
  if (result.value && lugares && lugares.length > 0) {
    result.value = "El objeto está en: " + formarString(lugares);
  } else {
    result.value = "El objeto no existe";
  }
} else if (predi === "que_tengo") {
  const lugares = result.value.data[0].X;
  if (result.value && lugares && lugares.length > 0) {
    result.value = "Tienes: " + formarString(lugares);
  } else {
    result.value = "No tienes ningún objeto";
  }
} else if (predi === "lugar_visitados") {
  const lugares = result.value.data[0].X;
  if (result.value && lugares && lugares.length > 0) {
    result.value = "Has visitado: " + formarString(lugares);
  } else {
    result.value = "No has visitado ningun lugar";
  }
} else if (predi === "ruta") {
  const lugares = result.value.data[0].X;
  if (result.value && lugares && lugares.length > 0) {
    result.value = "La ruta es la siguiente: " + lugares;
  } else {
    result.value = "No existe una ruta entre ambos lugares";
  }
} else if (predi === "como_gano"){
  const condiciones = result.value.data[0].X;
  if (result.value.resultado && condiciones && condiciones.length > 0) {
    result.value = "Para ganar tienes que hacer esto: " + formarComoGano(result.value.data[0].X);
  } else {
    result.value = "No existe forma de ganar en este momento.";
  }
}else{
  const X = result.value.data[0].X;
  if (X && X.length > 0) {
    result.value = "Has ganado!" + formarGane(X);
  } else {
    result.value = "No has ganado.";
  }

}

}


async function enviarSinParametros(value){
  campoActivo.value = null
  await enviarPredicado(value, ["X"])
  mostrarResultados(value)
}
</script>


<template>
  <div class="adventure-container">
    <h1 class="adventure-title">Bienvenido a Adventure</h1>
    <div class="log-box" :class="{ 'log-loading': loading, 'log-error': error, 'log-result': result && !error }">
      <template v-if="loading">Cargando...</template>
      <template v-else-if="error">Error: Parametro mal introducido</template>
      <template v-else-if="result">
        <h2>Resultado:</h2>
        <pre>{{ result }}</pre>
      </template>
      <template v-else>
        <span>Usa los controles para jugar y ver resultados aquí.</span>
      </template>
    </div>
    <div class="input-area">
      <form v-if="campoActivo === 1" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Nombre del objeto..." />
        <button class="game-btn input-btn" type="submit" :disabled="loading">Tomar</button>
      </form>
      <form v-if="campoActivo === 2" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Objeto a usar..." />
        <button class="game-btn input-btn" type="submit" :disabled="loading">Usar</button>
      </form>
      <form v-if="campoActivo === 3" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Hacia donde..." />
        <button class="game-btn input-btn" type="submit" :disabled="loading">Consultar</button>
      </form>
      <form v-if="campoActivo === 4" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Destino..." />
        <button class="game-btn input-btn" type="submit" :disabled="loading">Mover</button>
      </form>
      <form v-if="campoActivo === 5" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Objeto..." />
        <button class="game-btn input-btn" type="submit" :disabled="loading">Consultar</button>
      </form>
      <form v-if="campoActivo === 8" @submit.prevent="formarQuery" class="input-form-row">
        <input class="game-input" v-model="query" placeholder="Origen"/>
        <input class="game-input" v-model="query2" placeholder="Destino">
        <button class="game-btn input-btn" type="submit" :disabled="loading">Consultar ruta</button>
      </form>
    </div>
    <div class="botones-grid">
      <button class="game-btn" @click="mostrarCampos(1, 'tomar')">Tomar objeto</button>
      <button class="game-btn" @click="mostrarCampos(2, 'usar')">Usar objeto</button>
      <button class="game-btn" @click="enviarSinParametros('que_tengo')">Inventario</button>
      <button class="game-btn" @click="mostrarCampos(5, 'donde_esta')">¿Dónde está?</button>
      <button class="game-btn" @click="mostrarCampos(3, 'puedo_ir')">¿Puedo ir?</button>
      <button class="game-btn" @click="mostrarCampos(4, 'mover')">Mover</button>
      <button class="game-btn" @click="mostrarCampos(8, 'ruta')">Ruta</button>
      <button class="game-btn" @click="enviarSinParametros('lugar_visitados')">Lugares visitados</button>
      <button class="game-btn" @click="enviarSinParametros('como_gano')">¿Cómo gano?</button>
      <button class="game-btn" @click="enviarSinParametros('verifica_gane')">Verifica gane</button>
    </div>
  </div>
</template>

<style scoped>
/* Layout containers */
.adventure-container {
  max-width: 700px;
  margin: 0 auto;
  padding: 2.5em 0;
  display: flex;
  flex-direction: column;
  align-items: center;
}
.adventure-title {
  text-align: center;
  font-size: 2.5em;
  font-weight: 700;
  color: #181f2a;
  margin-bottom: 1.2em;
  letter-spacing: 2px;
  text-shadow: 0 2px 8px rgba(76, 175, 255, 0.12);
}
.log-box {
  background: #181f2a;
  color: #eaf1fb;
  border-radius: 12px;
  padding: 1.5em 1.2em;
  font-family: 'Fira Mono', 'Consolas', monospace;
  font-size: 1.08em;
  min-height: 180px;
  max-height: 350px;
  width: 100%;
  max-width: 600px;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.12);
  margin-bottom: 1.2em;
  white-space: pre-wrap;
  word-break: break-word;
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  overflow-y: auto;
  align-items: flex-start;
}
.input-area {
  width: 100%;
  max-width: 600px;
  border-radius: 8px;
  padding: 0 0 0.5em 0;
  margin-bottom: 1.2em;
  display: flex;
  flex-direction: column;
  align-items: center;
}
.input-form-row {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 0.7em;
  margin-bottom: 0.7em;
  width: 100%;
  justify-content: center;
}
.game-input {
  padding: 0.7em 1.1em;
  border: 1px solid #b3c6e0;
  border-radius: 8px;
  font-size: 1.08em;
  width: 230px;
  box-sizing: border-box;
  height: 48px;
}
.input-btn {
  height: 48px;
  margin: 0;
  background: #181f2a !important;
  color: #eaf1fb !important;
}
.botones-grid {
  width: 100%;
  max-width: 600px;
  border-radius: 8px;
  padding: 0;
  margin-bottom: 2em;
  box-sizing: border-box;
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 1em;
}
.game-btn {
  background: #181f2a;
  color: #eaf1fb;
  border: none;
  border-radius: 8px;
  padding: 0.7em 1.4em;
  font-weight: 600;
  font-size: 1.08em;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.15);
  cursor: pointer;
  transition: background 0.2s, transform 0.2s;
  margin-bottom: 0.2em;
  width: 170px;
  min-height: 60px;
  text-align: center;
  display: flex;
  align-items: center;
  justify-content: center;
}
.game-btn {
  background: #181f2a;
  color: #eaf1fb;
  border: none;
  border-radius: 8px;
  padding: 0.7em 1.4em;
  font-weight: 600;
  font-size: 1.08em;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.15);
  cursor: pointer;
  transition: background 0.2s, transform 0.2s;
  margin-bottom: 0.2em;
  width: 170px;
  min-height: 60px;
  text-align: center;
  display: flex;
  align-items: center;
  justify-content: center;
}
.game-btn:disabled {
  background: #b3c6e0 !important;
  color: #fff !important;
  cursor: not-allowed;
}
.game-btn:hover:not(:disabled) {
  background: #222a35 !important;
  transform: translateY(-2px);
}
.log-loading {
  color: #ffe066;
}
.log-error {
  color: #ff6b6b;
}
.log-result {
  color: #eaf1fb;
}
</style>
