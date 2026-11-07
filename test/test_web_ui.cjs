// Run with: node --test test/test_web_ui.cjs
const assert = require('node:assert/strict')
const { readFileSync } = require('node:fs')
const { test } = require('node:test')
const vm = require('node:vm')

const source = readFileSync(`${__dirname}/../web/static/app.js`, 'utf8')
const freshGame = () => ({
  size: 9, board: Array.from({ length: 9 }, () => Array(9).fill(null)),
  turn: 'black', status: 'Your turn — play Black.', moves: [], gameOver: false, revision: 0, score: null,
})
const json = body => new Response(JSON.stringify(body))
const settled = () => new Promise(resolve => setImmediate(resolve))

// Only the DOM surface used by the app; browser checks cover layout and real input.
function element() {
  return {
    classList: { toggle() {}, add() {} }, style: {}, dataset: {}, children: [], listeners: {},
    set innerHTML(value) { this.children = [] },
    set textContent(value) { this.text = String(value) },
    get textContent() { return this.text || '' },
    append(...children) { this.children.push(...children) },
    setAttribute() {},
    addEventListener(type, listener) { this.listeners[type] = listener },
  }
}

async function loadApp(fetch) {
  const elements = new Map()
  const document = {
    querySelector(selector) {
      if (!elements.has(selector)) elements.set(selector, element())
      return elements.get(selector)
    },
    createElement: element,
  }
  vm.runInNewContext(source, {
    document, fetch, AbortSignal,
    window: { clearTimeout() {}, setTimeout() {} },
  })
  await settled()
  return selector => document.querySelector(selector)
}

test('lost move response refreshes the committed game without replaying the move', async () => {
  let server = freshGame()
  let moves = 0
  let finishRefresh
  const $ = await loadApp(async path => {
    if (path === '/api/move') {
      moves++
      server = { ...server, revision: 2, gameOver: true, turn: 'none',
        status: 'Kurt resigned. You win.',
        moves: [{ color: 'black', vertex: 'PASS' }, { color: 'white', vertex: 'RESIGN' }] }
      return new Response('<h1>Gateway Timeout</h1>', { status: 504 })
    }
    if (moves) return new Promise(resolve => { finishRefresh = () => resolve(json(server)) })
    return json(server)
  })
  $('#pass').listeners.click()
  await settled()
  assert.equal($('#pass').disabled, true, 'do not allow stale play during recovery')
  assert.equal($('#reset').disabled, true)
  assert.ok(finishRefresh, 'failed POST must fetch authoritative state')
  finishRefresh()
  await settled()
  assert.equal($('#move-count').textContent, '2 moves')
  assert.equal($('#status').textContent, 'Kurt resigned. You win.')
  assert.equal($('#score').hidden, true, 'resignation is not a counted score')
  assert.equal($('#pass').disabled, true)
  assert.equal($('#reset').disabled, false)
  assert.match($('#toast').textContent, /504/)
  assert.equal(moves, 1)
})

test('failed resynchronization leaves stale play disabled but offers a new game', async () => {
  let failed = false
  const $ = await loadApp(async path => {
    if (path === '/api/move') failed = true
    if (failed) throw new TypeError('Failed to fetch')
    return json(freshGame())
  })
  $('#pass').listeners.click()
  await settled()
  assert.equal($('#pass').disabled, true)
  assert.equal($('#reset').disabled, false)
  assert.match($('#status').textContent, /Reload to recover/)
})

test('lost reset response recovers the fresh board and permits play', async () => {
  let server = { ...freshGame(), revision: 2, moves: [{ color: 'black', vertex: 'PASS' }, { color: 'white', vertex: 'E5' }] }
  const $ = await loadApp(async path => {
    if (path === '/api/reset') {
      server = freshGame()
      throw new TypeError('Failed to fetch')
    }
    return json(server)
  })
  await $('#reset').listeners.click()
  assert.equal($('#move-count').textContent, '0 moves')
  assert.equal($('#pass').disabled, false)
  assert.equal($('#reset').disabled, false)
})

test('counted finish shows area breakdown and a new game clears it', async () => {
  let server = { ...freshGame(), gameOver: true, turn: 'none', status: 'White wins by 8.5 points.',
    score: { black: { stones: 33, territory: 7, total: 40 },
      white: { stones: 35, territory: 6, total: 48.5 }, komi: 7.5, neutral: 0 } }
  const $ = await loadApp(async path => {
    if (path === '/api/reset') server = freshGame()
    return json(server)
  })
  assert.equal($('#score').hidden, false)
  assert.equal($('#black-stones').textContent, '33')
  assert.equal($('#black-territory').textContent, '7')
  assert.equal($('#black-total').textContent, '40')
  assert.equal($('#white-stones').textContent, '35')
  assert.equal($('#white-territory').textContent, '6')
  assert.equal($('#white-total').textContent, '48.5')
  assert.equal($('#komi').textContent, '7.5')
  assert.equal($('#neutral').textContent, '0')
  assert.equal($('#pass').disabled, true)
  await $('#reset').listeners.click()
  assert.equal($('#score').hidden, true)
  assert.equal($('#pass').disabled, false)
})
