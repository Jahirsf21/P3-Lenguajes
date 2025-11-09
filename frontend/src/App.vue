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
  <h1>Consulta Prolog</h1>


  <div class="campos">
    <form v-if="campoActivo === 1" @submit.prevent="formarQuery">
      <h2>TOMAR</h2>
      <input v-model="query" placeholder="Nombre del objeto..." />
      <button type="submit" :disabled="loading">Enviar</button>
    </form>

    <form v-if="campoActivo === 2" @submit.prevent="formarQuery">
      <h2>USAR</h2>
      <input v-model="query" placeholder="Objeto a usar..." />
      <button type="submit" :disabled="loading">Enviar</button>
    </form>

    <form v-if="campoActivo === 3" @submit.prevent="formarQuery">
      <h2>PUEDO_IR</h2>
      <input v-model="query" placeholder="Hacia donde..." />
      <button type="submit" :disabled="loading">Enviar</button>
    </form>

    <form v-if="campoActivo === 4" @submit.prevent="formarQuery">
      <h2>MOVER</h2>
      <input v-model="query" placeholder="Destino..." />
      <button type="submit" :disabled="loading">Enviar</button>
    </form>

    <form v-if="campoActivo === 5" @submit.prevent="formarQuery">
      <h2>DONDE_ESTA</h2>
      <input v-model="query" placeholder="Objeto..." />
      <button type="submit" :disabled="loading">Enviar</button>
    </form>


    <form v-if="campoActivo === 8" @submit.prevent="formarQuery">
      <h2>RUTA</h2>
      <input v-model="query" placeholder="Origen"/>
      <input v-model="query2" placeholder="Destino">
      <button type="submit" :disabled="loading">Enviar</button>
    </form>
    </div>

  <div class="botones">
    <button @click="mostrarCampos(1, 'tomar')">TOMAR</button>
    <button @click="mostrarCampos(2, 'usar')">USAR</button>
    <button @click="mostrarCampos(3, 'puedo_ir')">PUEDO_IR</button>
    <button @click="mostrarCampos(4, 'mover')">MOVER</button>
    <button @click="mostrarCampos(5, 'donde_esta')">DONDE_ESTA</button>
    <button @click="enviarSinParametros('que_tengo')">QUE_TENGO</button>
    <button @click="enviarSinParametros('lugar_visitados')">LUGAR_VISITADOS</button>
    <button @click="mostrarCampos(8, 'ruta')">RUTA</button>
    <button @click="enviarSinParametros('como_gano')">COMO_GANO</button>
    <button @click="enviarSinParametros('verifica_gane')">VERIFICA_GANE</button>
  </div>

  <div v-if="loading">Cargando...</div>
  <div v-if="error" style="color: red">Error: Parametro mal introducido</div>
  <div v-if="result && !error">
    <h2>Resultado:</h2>
    <pre>{{ result }}</pre>
  </div>
</template>

<style scoped>

</style>
