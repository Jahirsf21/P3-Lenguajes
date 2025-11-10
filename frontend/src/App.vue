<script setup>
import { ref, onMounted, nextTick } from 'vue';

const query = ref('');
const query2 = ref('');
const pred = ref('');
const results = ref([]);
const lugares = ref([]);
const objetosUsados = ref([]);
const todosLugares = ref([]);
const todosObjetos = ref([]);
const loading = ref(false);
const error = ref('');
const campoActivo = ref(null);
const resultsEndRef = ref(null);

const scrollToBottom = async () => {
  await nextTick();
  resultsEndRef.value?.scrollIntoView({ behavior: "smooth" });
};

onMounted(() => {
  try {
    const cachedLugares = localStorage.getItem('lugaresVisited');
    if (cachedLugares) {
      lugares.value = JSON.parse(cachedLugares);
    } else {
      actualizarLugares();
    }

    const cachedObjetosUsados = localStorage.getItem('objetosUsados');
    if (cachedObjetosUsados) {
      objetosUsados.value = JSON.parse(cachedObjetosUsados);
    } else {
      actualizarObjetosUsados();
    }

    const cachedTodosLugares = localStorage.getItem('todosLugares');
    if (cachedTodosLugares) {
      todosLugares.value = JSON.parse(cachedTodosLugares);
    } else {
      fetchTodosLugares();
    }

    const cachedTodosObjetos = localStorage.getItem('todosObjetos');
    if (cachedTodosObjetos) {
      todosObjetos.value = JSON.parse(cachedTodosObjetos);
    } else {
      fetchTodosObjetos();
    }
  } catch (e) {
    actualizarLugares();
    actualizarObjetosUsados();
    fetchTodosLugares();
    fetchTodosObjetos();
  }
});

async function enviarPredicado(predicado, parametros = []) {
  loading.value = true;
  error.value = '';
  try {
    const response = await fetch('http://127.0.0.1:5000/query', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ predicado, parametros })
    });
    const data = await response.json();
    if (!response.ok) {
      error.value = data.error || 'Error desconocido';
      return null;
    }
    return data;
  } catch (e) {
    error.value = e.message;
    return null;
  } finally {
    loading.value = false;
  }
}

async function actualizarLugares() {
  const data = await enviarPredicado('lugar_visitados', ['X']);
  if (data?.data?.[0]) {
    lugares.value = data.data[0].X || [];
    try { localStorage.setItem('lugaresVisited', JSON.stringify(lugares.value)); } catch (e) {}
  }
}

async function actualizarObjetosUsados() {
  const data = await enviarPredicado('objetos_usados', ['X']);
  if (data?.data?.[0]) {
    objetosUsados.value = data.data[0].X || [];
    try { localStorage.setItem('objetosUsados', JSON.stringify(objetosUsados.value)); } catch (e) {}
  }
}

async function fetchTodosLugares() {
  const data = await enviarPredicado('lugar', ['X', '_']);
  if (data?.data) {
    todosLugares.value = data.data.map(item => item.X);
    try { localStorage.setItem('todosLugares', JSON.stringify(todosLugares.value)); } catch (e) {}
  }
}

async function fetchTodosObjetos() {
  const data = await enviarPredicado('objeto', ['X', '_']);
  if (data?.data) {
    todosObjetos.value = data.data.map(item => item.X);
    try { localStorage.setItem('todosObjetos', JSON.stringify(todosObjetos.value)); } catch (e) {}
  }
}

function mostrarCampos(numero, predi) {
  campoActivo.value = numero;
  pred.value = predi;
}

async function formarQuery() {
  const parametros = [];
  if (query.value) parametros.push(query.value.toLowerCase());
  if (query2.value) parametros.push(query2.value.toLowerCase());
  if (pred.value === "donde_esta" || pred.value === "ruta") parametros.push("X");

  const data = await enviarPredicado(pred.value, parametros);
  if (data) {
    const mensaje = mostrarResultados(pred.value, data);
    results.value.push({ predicado: pred.value, mensaje, timestamp: new Date() });
    scrollToBottom();

    if (['mover', 'tomar', 'usar'].includes(pred.value)) {
      await actualizarLugares();
      await actualizarObjetosUsados();
    }
  }
  query.value = '';
  query2.value = '';
  campoActivo.value = null;
}

function formarString(lista) {
  if (!lista || lista.length === 0) return "";
  return lista.join(", ");
}

function formarComoGano(lista) {
  if (!lista || lista.length === 0) return "";
  let resultado = "\n";
  lista.forEach((r, i) => {
    const [lugarFinal, objeto, camino] = r;
    resultado += `${i + 1}. Llegar a ${lugarFinal} con ${objeto} e ir por: ${camino.join(" -> ")}\n`;
  });
  return resultado;
}

function formarGane(lista) {
  const [camino, inventario, condiciones] = lista;
  let mensaje = "\n";
  mensaje += "Camino: " + camino.join(" -> ") + "\n";
  mensaje += "Inventario: " + inventario.join(", ") + "\n";
  mensaje += "Condiciones cumplidas: " + condiciones.join(", ");
  return mensaje;
}

function mostrarResultados(predi, result) {
  if (predi === "tomar") {
    return result.resultado ? "El objeto se ha tomado correctamente" : "No se pudo tomar el objeto";
  } else if (predi === "usar") {
    return result.resultado ? "El objeto se ha usado correctamente" : "No se pudo usar el objeto";
  } else if (predi === "puedo_ir") {
    return result.resultado ? "Sí puedes ir a ese lugar" : "No puedes ir, no cumples las condiciones";
  } else if (predi === "mover") {
    return result.resultado ? "Te has movido a ese lugar" : "No puedes ir, no cumples las condiciones";
  } else if (predi === "donde_esta") {
    const lugares = result.data[0]?.X;
    return lugares?.length > 0 ? "El objeto está en: " + formarString(lugares) : "El objeto no existe";
  } else if (predi === "que_tengo") {
    const items = result.data[0]?.X;
    return items?.length > 0 ? "Tienes: " + formarString(items) : "No tienes ningún objeto";
  } else if (predi === "lugar_visitados") {
    const lugares = result.data[0]?.X;
    return lugares?.length > 0 ? "Has visitado: " + formarString(lugares) : "No has visitado ningún lugar";
  } else if (predi === "ruta") {
    const ruta = result.data[0]?.X;
    return ruta?.length > 0 ? "La ruta: " + ruta.join(" -> ") : "No existe una ruta";
  } else if (predi === "como_gano") {
    const condiciones = result.data[0]?.X;
    return condiciones?.length > 0 ? "Para ganar:" + formarComoGano(condiciones) : "No existe forma de ganar";
  } else {
    const X = result.data[0]?.X;
    return X?.length > 0 ? "¡Has ganado!" + formarGane(X) : "No has ganado";
  }
}

async function enviarSinParametros(value) {
  campoActivo.value = null;
  const data = await enviarPredicado(value, ["X"]);
  if (data) {
    const mensaje = mostrarResultados(value, data);
    results.value.push({ predicado: value, mensaje, timestamp: new Date() });
    scrollToBottom();
  }
}
</script>

<template>
  <div class="container">
    <h1 class="title">Bienvenido a Adventure</h1>

    <div class="main-layout">
      <!-- Columna Izquierda: Estado del jugador -->
      <div class="side-column">
        <div class="side-log">
          <h3 class="side-log-title">Lugares Visitados</h3>
          <div class="side-log-content">
            <div v-if="lugares.length === 0" class="empty-state">
              No has visitado ningún lugar
            </div>
            <div v-else>
              <div v-for="(lugar, i) in lugares" :key="i" class="log-item">
                {{ i + 1 }}. {{ lugar }}
              </div>
            </div>
          </div>
        </div>

        <div class="side-log">
          <h3 class="side-log-title">Objetos Usados</h3>
          <div class="side-log-content">
            <div v-if="objetosUsados.length === 0" class="empty-state">
              No has usado ningún objeto
            </div>
            <div v-else>
              <div v-for="(objeto, i) in objetosUsados" :key="i" class="log-item">
                {{ i + 1 }}. {{ objeto }}
              </div>
            </div>
          </div>
        </div>
      </div>

      <!-- Columna Central: Controles y Resultados -->
      <div class="center-column">
        <div class="log-box" :class="{ 'log-loading': loading, 'log-error': error }">
          <div v-if="loading" class="loading-text">Cargando...</div>
          <div v-else-if="error" class="error-text">Error: Parámetro mal introducido</div>
          <div v-else-if="results.length === 0" class="empty-state">
            Usa los controles para jugar y ver resultados aquí.
          </div>
          <div v-else>
            <div v-for="(result, i) in results" :key="i" class="result-item">
              <div class="result-header">
                <span class="result-command">→ {{ result.predicado }}</span>
                <span class="result-time">
                  {{ result.timestamp.toLocaleTimeString() }}
                </span>
              </div>
              <pre class="result-message">{{ result.mensaje }}</pre>
              <div v-if="i < results.length - 1" class="separator" />
            </div>
            <div ref="resultsEndRef" />
          </div>
        </div>

        <div class="input-area">
          <form @submit.prevent="formarQuery" v-if="campoActivo !== null" class="input-form">
            <template v-if="campoActivo === 1">
              <input
                v-model="query"
                placeholder="Nombre del objeto..."
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Tomar</button>
            </template>
            <template v-if="campoActivo === 2">
              <input
                v-model="query"
                placeholder="Objeto a usar..."
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Usar</button>
            </template>
            <template v-if="campoActivo === 3">
              <input
                v-model="query"
                placeholder="Hacia donde..."
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Consultar</button>
            </template>
            <template v-if="campoActivo === 4">
              <input
                v-model="query"
                placeholder="Destino..."
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Mover</button>
            </template>
            <template v-if="campoActivo === 5">
              <input
                v-model="query"
                placeholder="Objeto..."
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Consultar</button>
            </template>
            <template v-if="campoActivo === 8">
              <input v-model="query" placeholder="Origen" class="input" />
              <input
                v-model="query2"
                placeholder="Destino"
                @keydown.enter="formarQuery"
                class="input"
              />
              <button type="submit" :disabled="loading" class="input-btn">Consultar ruta</button>
            </template>
          </form>
        </div>

        <div class="buttons-wrapper">
          <div class="buttons-group">
            <h3 class="buttons-title">Acciones</h3>
            <div class="botones-grid">
              <button @click="mostrarCampos(1, 'tomar')" class="game-btn">Tomar objeto</button>
              <button @click="mostrarCampos(2, 'usar')" class="game-btn">Usar objeto</button>
              <button @click="mostrarCampos(4, 'mover')" class="game-btn">Mover</button>
            </div>
          </div>

          <div class="buttons-group">
            <h3 class="buttons-title">Consultas</h3>
            <div class="botones-grid">
              <button @click="enviarSinParametros('que_tengo')" class="game-btn">Inventario</button>
              <button @click="mostrarCampos(5, 'donde_esta')" class="game-btn">¿Dónde está?</button>
              <button @click="mostrarCampos(3, 'puedo_ir')" class="game-btn">¿Puedo ir?</button>
              <button @click="mostrarCampos(8, 'ruta')" class="game-btn">Ruta</button>
              <button @click="enviarSinParametros('lugar_visitados')" class="game-btn">
                Lugares visitados
              </button>
              <button @click="enviarSinParametros('como_gano')" class="game-btn">¿Cómo gano?</button>
              <button @click="enviarSinParametros('verifica_gane')" class="game-btn">
                Verifica gane
              </button>
            </div>
          </div>
        </div>
      </div>

      <!-- Columna Derecha: Información global -->
      <div class="side-column">
        <div class="side-log">
          <h3 class="side-log-title">Todos los Lugares</h3>
          <div class="side-log-content">
            <div v-if="todosLugares.length === 0" class="empty-state">
              No hay lugares definidos
            </div>
            <div v-else>
              <div v-for="(lugar, i) in todosLugares" :key="i" class="log-item">
                {{ i + 1 }}. {{ lugar }}
              </div>
            </div>
          </div>
        </div>

        <div class="side-log">
          <h3 class="side-log-title">Todos los Objetos</h3>
          <div class="side-log-content">
            <div v-if="todosObjetos.length === 0" class="empty-state">
              No hay objetos definidos
            </div>
            <div v-else>
              <div v-for="(objeto, i) in todosObjetos" :key="i" class="log-item">
                {{ i + 1 }}. {{ objeto }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 2rem;
  font-family: 'Fira Mono', 'Consolas', monospace;
}
.title {
  text-align: center;
  font-size: 2.5rem;
  font-weight: 700;
  color: #181f2a;
  margin-bottom: 2rem;
  letter-spacing: 2px;
  text-shadow: 0 2px 8px rgba(76, 175, 255, 0.12);
}
.main-layout {
  display: flex;
  gap: 1.5rem;
  align-items: flex-start;
}
.side-column {
  flex: 0 0 260px;
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
.side-log {
  background: #181f2a;
  border-radius: 12px;
  padding: 1rem;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.12);
  max-height: 290px;
  display: flex;
  flex-direction: column;
}
.side-log-title {
  color: #eaf1fb;
  font-size: 0.95rem;
  margin: 0 0 0.75rem 0;
  border-bottom: 2px solid #2a3441;
  padding-bottom: 0.4rem;
  text-transform: uppercase;
  letter-spacing: 1px;
}
.side-log-content {
  color: #eaf1fb;
  font-size: 0.9rem;
  overflow-y: auto;
  flex: 1;
}
.log-item {
  padding: 0.4rem 0.5rem;
  margin-bottom: 0.4rem;
  background: #222a35;
  border-radius: 6px;
  border-left: 3px solid #4cafff;
}
.center-column {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}
.log-box {
  background: #181f2a;
  color: #eaf1fb;
  border-radius: 12px;
  padding: 1.5rem;
  font-size: 1rem;
  min-height: 300px;
  max-height: 400px;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.12);
  overflow-y: auto;
}
.loading-text {
  color: #ffe066;
  font-size: 1.1rem;
}
.error-text {
  color: #ff6b6b;
  font-size: 1.1rem;
}
.empty-state {
  color: #8a96a3;
  font-style: italic;
  text-align: center;
  padding: 2rem;
}
.result-item {
  margin-bottom: 1rem;
}
.result-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.5rem;
}
.result-command {
  color: #4cafff;
  font-weight: bold;
}
.result-time {
  color: #8a96a3;
  font-size: 0.85rem;
}
.result-message {
  margin: 0;
  white-space: pre-wrap;
  word-break: break-word;
  color: #eaf1fb;
  font-size: 0.95rem;
}
.separator {
  height: 1px;
  background: #2a3441;
  margin: 1rem 0;
}
.input-area {
  min-height: 60px;
}
.input-form {
  display: flex;
  gap: 0.7rem;
  align-items: center;
  flex-wrap: wrap;
}
.input {
  padding: 0.7rem 1.1rem;
  border: 1px solid #b3c6e0;
  border-radius: 8px;
  font-size: 1rem;
  flex: 1;
  min-width: 180px;
  max-width: 260px;
}
.input-btn {
  background: #181f2a;
  color: #eaf1fb;
  border: none;
  border-radius: 8px;
  padding: 0.7rem 1.4rem;
  font-weight: 600;
  font-size: 1rem;
  cursor: pointer;
  transition: all 0.2s;
  min-width: 120px;
}
.input-btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.3);
}
.buttons-wrapper {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
.buttons-group {
  background: #181f2a;
  border-radius: 12px;
  padding: 1rem;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.12);
}
.buttons-title {
  margin: 0 0 0.75rem 0;
  font-size: 0.9rem;
  color: #eaf1fb;
  text-transform: uppercase;
  letter-spacing: 1px;
  opacity: 0.85;
}
.botones-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
  gap: 0.75rem;
}
.game-btn {
  background: #181f2a;
  color: #eaf1fb;
  border: 1px solid #2a3441;
  border-radius: 8px;
  padding: 0.9rem 1rem;
  font-weight: 600;
  font-size: 0.95rem;
  box-shadow: 0 2px 8px rgba(24, 31, 42, 0.15);
  cursor: pointer;
  transition: all 0.2s;
  min-height: 52px;
  display: flex;
  align-items: center;
  justify-content: center;
}
.game-btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 3px 10px rgba(24, 31, 42, 0.35);
}
.log-loading {
  border-left: 4px solid #ffe066;
}
.log-error {
  border-left: 4px solid #ff6b6b;
}
</style>