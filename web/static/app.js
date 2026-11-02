const boardElement = document.querySelector('#board')
const statusElement = document.querySelector('#status')
const turnDot = document.querySelector('#turn-dot')
const humanPlayer = document.querySelector('#human-player')
const kurtPlayer = document.querySelector('#kurt-player')
const movesElement = document.querySelector('#moves')
const moveCount = document.querySelector('#move-count')
const passButton = document.querySelector('#pass')
const resetButton = document.querySelector('#reset')
const toast = document.querySelector('#toast')
const scoreElement = document.querySelector('#score')

let state = null
let busy = true

function showError(message) {
  toast.textContent = message
  toast.hidden = false
  turnDot.className = 'turn-dot error'
  window.clearTimeout(showError.timer)
  showError.timer = window.setTimeout(() => { toast.hidden = true }, 5000)
}

async function request(path, options = {}) {
  const response = await fetch(path, {
    ...options,
    headers: options.body ? { 'Content-Type': 'application/json' } : undefined,
    signal: AbortSignal.timeout(30000),
  })
  if (!response.ok) {
    // Proxy errors may be HTML or plain text, rather than bridge JSON.
    const body = await response.json().catch(() => ({}))
    throw new Error(body.error || `Request failed (${response.status})`)
  }
  return response.json()
}

async function recover(error) {
  statusElement.textContent = 'Checking the current game…'
  try {
    // A failed response does not mean the move or reset failed on the server.
    const nextState = await request('/api/state')
    busy = false
    render(nextState)
    showError(`${error.message} — current game recovered.`)
  } catch {
    statusElement.textContent = 'Connection lost. Reload to recover this game, or start a new game.'
    resetButton.disabled = false
    showError(error.message)
    // Keep play locked until the server's state is known.
  }
}

function render(nextState) {
  state = nextState
  const canPlay = !busy && state.turn === 'black' && !state.gameOver
  boardElement.classList.toggle('ready', canPlay)
  boardElement.innerHTML = ''
  const lastMove = [...state.moves].reverse().find(move => !['PASS', 'RESIGN'].includes(move.vertex))

  for (let index = 0; index < state.size; index++) {
    const column = document.createElement('span')
    column.className = 'coordinate column'
    column.textContent = 'ABCDEFGHJ'[index]
    column.style.left = `${8 + index * 10.5}%`
    column.setAttribute('aria-hidden', 'true')
    const row = document.createElement('span')
    row.className = 'coordinate row'
    row.textContent = state.size - index
    row.style.top = `${8 + index * 10.5}%`
    row.setAttribute('aria-hidden', 'true')
    boardElement.append(column, row)
  }

  state.board.forEach((row, rowIndex) => row.forEach((value, colIndex) => {
    const point = document.createElement('button')
    point.type = 'button'
    point.className = 'point'
    point.setAttribute('role', 'gridcell')
    point.dataset.row = rowIndex
    point.dataset.col = colIndex
    point.style.left = `${8 + colIndex * 10.5}%`
    point.style.top = `${8 + rowIndex * 10.5}%`
    const vertex = `${'ABCDEFGHJKLMNOPQRSTUVWXYZ'[colIndex]}${state.size - rowIndex}`
    point.setAttribute('aria-label', value ? `${vertex}, ${value} stone` : `Play ${vertex}`)
    point.disabled = !canPlay || value !== null
    if (value) {
      point.classList.add('occupied')
      const stone = document.createElement('span')
      stone.className = `stone ${value}`
      point.append(stone)
    } else if ([2, 4, 6].includes(rowIndex) && [2, 4, 6].includes(colIndex)) {
      point.classList.add('star')
    }
    if (lastMove?.vertex === vertex) point.classList.add('last')
    boardElement.append(point)
  }))

  statusElement.textContent = busy && state.turn === 'white' ? 'Kurt is thinking…' : state.status
  turnDot.className = `turn-dot ${state.turn === 'white' ? 'white' : ''}`
  humanPlayer.classList.toggle('active', state.turn === 'black')
  kurtPlayer.classList.toggle('active', state.turn === 'white')
  passButton.disabled = !canPlay
  resetButton.disabled = busy
  scoreElement.hidden = !state.score
  if (state.score) {
    for (const color of ['black', 'white']) {
      for (const field of ['stones', 'territory', 'total']) {
        document.querySelector(`#${color}-${field}`).textContent = state.score[color][field]
      }
    }
    document.querySelector('#komi').textContent = state.score.komi
    document.querySelector('#neutral').textContent = state.score.neutral
  }
  moveCount.textContent = `${state.moves.length} ${state.moves.length === 1 ? 'move' : 'moves'}`
  movesElement.innerHTML = ''
  if (!state.moves.length) {
    movesElement.innerHTML = '<li class="empty">The first move is yours.</li>'
  } else {
    state.moves.forEach(move => {
      const item = document.createElement('li')
      item.innerHTML = `<span class="move-color ${move.color}"></span>${move.vertex}`
      movesElement.append(item)
    })
    movesElement.scrollTop = movesElement.scrollHeight
  }
}

async function play(row, col) {
  if (busy || !state) return
  busy = true
  toast.hidden = true
  render({ ...state, turn: 'white', status: 'Kurt is thinking…' })
  try {
    const nextState = await request('/api/move', {
      method: 'POST',
      body: JSON.stringify({ row, col, revision: state.revision }),
    })
    busy = false
    render(nextState)
  } catch (error) {
    await recover(error)
  }
}

boardElement.addEventListener('click', event => {
  const point = event.target.closest('.point')
  if (point) play(Number(point.dataset.row), Number(point.dataset.col))
})

passButton.addEventListener('click', () => play(null, null))

resetButton.addEventListener('click', async () => {
  busy = true
  toast.hidden = true
  if (state) render(state)
  resetButton.disabled = true
  statusElement.textContent = 'Starting a fresh game…'
  try {
    const nextState = await request('/api/reset', { method: 'POST' })
    busy = false
    render(nextState)
  } catch (error) {
    await recover(error)
  }
})

request('/api/state')
  .then(nextState => { busy = false; render(nextState) })
  .catch(error => {
    statusElement.textContent = 'Could not connect to Kurt. Reload to try again.'
    resetButton.disabled = false
    showError(error.message)
  })
