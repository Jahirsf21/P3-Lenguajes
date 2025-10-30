<script setup>
import { ref } from 'vue'

const query = ref('')
const result = ref(null)
const loading = ref(false)
const error = ref('')

async function sendQuery() {
  loading.value = true
  error.value = ''
  result.value = null
  try {
    const response = await fetch('http://127.0.0.1:5000/query', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ query: query.value }),
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
</script>

<template>
  <h1>Consulta Prolog</h1>
  <form @submit.prevent="sendQuery">
    <input v-model="query" placeholder="Escribe tu consulta Prolog" />
    <button type="submit" :disabled="loading">Enviar</button>
  </form>
  <div v-if="loading">Cargando...</div>
  <div v-if="error" style="color: red">Error: {{ error }}</div>
  <div v-if="result">
    <h2>Resultado:</h2>
    <pre>{{ result }}</pre>
  </div>
</template>

<style scoped>
form {
  margin-bottom: 1em;
}
input {
  padding: 0.5em;
  margin-right: 0.5em;
}
button {
  padding: 0.5em 1em;
}
</style>
