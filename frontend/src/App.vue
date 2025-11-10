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

function saveResults() {
  try { localStorage.setItem('results', JSON.stringify(results.value)); } catch (e) {}
}

function loadResultsFromStorage() {
  try {
    const raw = localStorage.getItem('results');
    if (!raw) return;
    const parsed = JSON.parse(raw);
    results.value = parsed.map(r => ({ ...r, timestamp: r.timestamp ? new Date(r.timestamp) : new Date() }));
  } catch (e) {
  }
}

onMounted(async () => {
  try {
    try {
      const vResp = await fetch('http://127.0.0.1:5000/version');
      if (vResp.ok) {
        const vJson = await vResp.json();
        const serverVersion = vJson.version;
        const cachedServerVersion = localStorage.getItem('serverVersion');
        if (cachedServerVersion !== serverVersion) {
          localStorage.removeItem('lugaresVisited');
          localStorage.removeItem('objetosUsados');
          localStorage.removeItem('results');
          localStorage.setItem('serverVersion', serverVersion);
        } else {
          loadResultsFromStorage();
        }
      }
    } catch (e) {
      console.warn('Could not fetch server version:', e?.message || e);
      loadResultsFromStorage();
    }

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
  const returned = data?.data?.[0]?.X;
  if (returned && Array.isArray(returned) && returned.length > 0) {
    lugares.value = returned;
    try { localStorage.setItem('lugaresVisited', JSON.stringify(lugares.value)); } catch (e) {}
    return;
  }
  try {
    const cached = localStorage.getItem('lugaresVisited');
    if (cached) {
      lugares.value = JSON.parse(cached);
      return;
    }
  } catch (e) {}
  lugares.value = [];
}

async function actualizarObjetosUsados() {
  const data = await enviarPredicado('objetos_usados', ['X']);
  const returned = data?.data?.[0]?.X;
  if (returned && Array.isArray(returned) && returned.length > 0) {
    objetosUsados.value = returned;
    try { localStorage.setItem('objetosUsados', JSON.stringify(objetosUsados.value)); } catch (e) {}
    return;
  }
  
  try {
    const cached = localStorage.getItem('objetosUsados');
    if (cached) {
      objetosUsados.value = JSON.parse(cached);
      return;
    }
  } catch (e) {}
  objetosUsados.value = [];
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

  const requiereParams = ["tomar", "usar", "mover", "puedo_ir", "donde_esta", "ruta"];
  if (requiereParams.includes(pred.value)) {
    const paramsSinX = parametros.filter(p => p !== "X");
    if (paramsSinX.length === 0 || paramsSinX.every(p => !p || p.trim() === "")) {
      error.value = "Debes completar los campos antes de consultar.";
      return;
    }
  }

  const data = await enviarPredicado(pred.value, parametros);
  if (data) {
    const mensaje = mostrarResultados(pred.value, data);
    results.value.push({ predicado: pred.value, mensaje, timestamp: new Date() });
    saveResults();
    scrollToBottom();

    if (["mover", "tomar", "usar"].includes(pred.value)) {
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
    resultado += ${i + 1}. Llegar a ${lugarFinal} con ${objeto} e ir por: ${camino.join(" -> ")}\n;
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
    saveResults();
    scrollToBottom();
  }
}
</script>

<template>
  <div class="container">
    <h1 class="title">Bienvenido a Adventure</h1>

    <div class="main-layout">
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
.page-bg {
  min-height: 100vh;
  padding: 2rem 0;
  background: radial-gradient(circle at top, #6b4423 0, #2b1a0f 45%, #140b06 100%);
  display: flex;
  justify-content: center;
  align-items: flex-start;
}
.container {
  max-width: 1400px;
  width: 95%;
  margin: 0 auto;
  padding: 2rem;
  font-family: 'Georgia', 'Times New Roman', serif;
  background: #f1e2c4;
  border-radius: 18px;
  border: 4px solid #c59b52;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.6);
  position: relative;
  overflow: hidden;
}
.container::before {
  content: "";
  position: absolute;
  inset: 0;
  background-image:
    radial-gradient(circle at 10% 0%, rgba(0, 0, 0, 0.08), transparent 60%),
    radial-gradient(circle at 90% 100%, rgba(0, 0, 0, 0.12), transparent 60%);
  mix-blend-mode: multiply;
  pointer-events: none;
}
.header {
  position: relative;
  text-align: center;
  margin-bottom: 2rem;
}
.header-glow {
  position: absolute;
  inset: 0;
  background: radial-gradient(circle at top, rgba(255, 215, 128, 0.4), transparent 60%);
  z-index: 0;
}
.title {
  position: relative;
  z-index: 1;
  font-size: 2.6rem;
  font-weight: 800;
  color: #3a1b0a;
  margin-bottom: 0.5rem;
  letter-spacing: 2px;
  text-shadow: 0 2px 4px rgba(80, 40, 10, 0.4);
}
.subtitle {
  position: relative;
  z-index: 1;
  color: #6a4120;
  font-size: 1rem;
  font-style: italic;
}
.main-layout {
  position: relative;
  z-index: 1;
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
  background: linear-gradient(145deg, #3f2614, #2a170d);
  border-radius: 14px;
  padding: 1rem;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.45);
  max-height: 290px;
  display: flex;
  flex-direction: column;
  border: 2px solid #c59b52;
}
.side-log-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin-bottom: 0.75rem;
  border-bottom: 1px solid rgba(255, 221, 157, 0.3);
  padding-bottom: 0.4rem;
}
.side-log-icon {
  font-size: 1.2rem;
}
.side-log-title {
  color: #ffe4b8;
  font-size: 0.95rem;
  margin: 0;
  text-transform: uppercase;
  letter-spacing: 1px;
}
.side-log-content {
  color: #fce5c8;
  font-size: 0.9rem;
  overflow-y: auto;
  flex: 1;
  scrollbar-width: none;
  -ms-overflow-style: none;
}
.side-log-content::-webkit-scrollbar {
  display: none;
}

.log-item {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.4rem 0.5rem;
  margin-bottom: 0.35rem;
  background: rgba(8, 5, 3, 0.4);
  border-radius: 8px;
  border-left: 3px solid #f6c453;
}
.log-index {
  font-weight: 700;
  color: #fcd38b;
}
.log-text {
  flex: 1;
}
.center-column {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}
.log-section {
  background: #f7ebcf;
  border-radius: 16px;
  padding: 1.2rem 1.4rem 1.4rem;
  border: 2px solid #d1a457;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.6);
}
.log-section-header {
  display: flex;
  align-items: center;
  gap: 0.8rem;
  margin-bottom: 1rem;
}
.scroll-icon {
  font-size: 1.8rem;
}
.log-title {
  margin: 0;
  font-size: 1.3rem;
  color: #3a1b0a;
  font-weight: 700;
}
.log-subtitle {
  margin: 0;
  font-size: 0.9rem;
  color: #7b5530;
}
.log-box {
  background: #261308;
  color: #ffe7c2;
  border-radius: 12px;
  padding: 1.1rem 1.2rem;
  font-size: 0.96rem;
  min-height: 260px;
  max-height: 360px;
  box-shadow: inset 0 0 0 1px rgba(255, 225, 177, 0.3);
  overflow-y: auto;
  position: relative;
  scrollbar-width: none;
  -ms-overflow-style: none;
}
.log-box::-webkit-scrollbar {
  display: none;
}

.loading-text {
  color: #ffe066;
  font-size: 1.05rem;
}
.error-text {
  color: #ff9b8b;
  font-size: 1.05rem;
}
.empty-state {
  color: #c9a27b;
  font-style: italic;
  text-align: center;
  padding: 2rem 1rem;
}
.result-item {
  margin-bottom: 1rem;
}
.result-header {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.4rem;
  font-size: 0.88rem;
}
.result-command {
  color: #f6c453;
  font-weight: 700;
}
.result-time {
  color: #c19b78;
}
.result-message {
  margin: 0;
  white-space: pre-wrap;
  word-break: break-word;
  color: #ffe9c7;
  font-size: 0.93rem;
}
.separator {
  height: 1px;
  background: rgba(206, 153, 88, 0.4);
  margin: 0.8rem 0;
}
.input-area {
  min-height: 60px;
}
.input-form {
  display: flex;
  gap: 0.7rem;
  align-items: center;
  flex-wrap: wrap;
  margin-top: 0.3rem;
}
.input {
  padding: 0.7rem 1.1rem;
  border: 1px solid #d1a457;
  border-radius: 999px;
  font-size: 0.98rem;
  flex: 1;
  min-width: 190px;
  max-width: 260px;
  background: #fff6e2;
  color: #3a1b0a;
}
.input:focus {
  outline: none;
  box-shadow: 0 0 0 2px rgba(237, 196, 114, 0.8);
}
.input-btn {
  background: linear-gradient(135deg, #d49b3f, #b6741e);
  color: #2b1608;
  border: none;
  border-radius: 999px;
  padding: 0.7rem 1.6rem;
  font-weight: 700;
  font-size: 0.96rem;
  cursor: pointer;
  transition: all 0.18s ease-out;
  min-width: 130px;
  box-shadow: 0 4px 0 #7a4b16;
}
.input-btn:hover {
  transform: translateY(-1px);
  box-shadow: 0 6px 0 #7a4b16;
}
.input-btn:active {
  transform: translateY(1px);
  box-shadow: 0 2px 0 #7a4b16;
}
.buttons-wrapper {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}
.buttons-group {
  background: #f7ebcf;
  border-radius: 16px;
  padding: 1rem 1.2rem 1.2rem;
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.25);
  border: 2px solid #d1a457;
}
.buttons-title {
  margin: 0 0 0.75rem 0;
  font-size: 0.97rem;
  color: #3a1b0a;
  text-transform: uppercase;
  letter-spacing: 1px;
}
.botones-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  gap: 0.7rem;
}
.game-btn {
  background: linear-gradient(135deg, #4b2915, #2f190c);
  color: #ffe9c5;
  border: 1px solid #f0c36b;
  border-radius: 999px;
  padding: 0.7rem 1rem;
  font-weight: 600;
  font-size: 0.95rem;
  box-shadow: 0 4px 10px rgba(0, 0, 0, 0.4);
  cursor: pointer;
  transition: all 0.18s ease-out;
  min-height: 48px;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.4rem;
}
.game-btn:hover {
  transform: translateY(-1px) scale(1.02);
  box-shadow: 0 6px 14px rgba(0, 0, 0, 0.6);
  filter: brightness(1.05);
}
.log-loading {
  box-shadow: 0 0 0 2px #ffe066;
}
.log-error {
  box-shadow: 0 0 0 2px #ff9b8b;
}
@media (max-width: 1024px) {
  .main-layout {
    flex-direction: column;
  }
  .side-column {
    flex: 1;
    max-width: 100%;
  }
}
@media (max-width: 640px) {
  .container {
    padding: 1.5rem 1.1rem;
  }
  .title {
    font-size: 2.1rem;
  }
  .botones-grid {
    grid-template-columns: 1fr;
  }
}
</style>