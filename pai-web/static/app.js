// pai-web main: state, events from Emacs, routing, attention.
'use strict';

const S = {
  client: null, poller: null,
  instances: [], byId: new Map(),
  chrome: {},             // instance id -> detailed state
  current: null,          // instance shown in the chat pane
  buffer: null,           // remote buffer shown
  prompts: new Map(),     // forwarded minibuffer prompts
  asks: [],               // ask_user questions
  buffers: [],            // Emacs buffers the page may show
  levels: [], colors: {},
  attention: new Set(),   // instances that finished while you were away
  lastAction: 0,
};

const App = (() => {
  function instTitle(inst) {
    if (!inst) return '';
    if (inst.title) return inst.title;
    return inst.name.replace(/^\*pai:?\s*/, '').replace(/\*$/, '') || inst.name;
  }

  async function act(body) {
    S.lastAction = Date.now();
    try {
      await Net.post('/api/action', Object.assign({ c: S.client }, body));
    } catch (e) { UI.toast(e.message, 'error'); }
  }

  function sendView() {
    if (!S.client) return;
    Net.post('/api/view', { c: S.client, i: S.current, buffers: S.buffer ? [S.buffer] : [] })
      .catch(() => {});
  }

  function setInstances(list) {
    const prev = S.byId;
    S.instances = list;
    S.byId = new Map(list.map(i => [i.id, i]));
    for (const inst of list) {
      const old = prev.get(inst.id);
      if (!old) continue;
      const away = document.hidden || S.current !== inst.id;
      if (old.active && !inst.active) {
        if (away) S.attention.add(inst.id);
        if (away || document.hidden) UI.notify(`${instTitle(inst)} is ready`, inst.status, () => openChat(inst.id));
      }
    }
    for (const id of [...S.attention]) if (!S.byId.has(id)) S.attention.delete(id);
    if (S.current && !S.byId.has(S.current)) {
      UI.toast('That instance was closed', 'warning');
      openList();
    }
    Chat.renderList();
    Chat.renderHeader();
    updateTitle();
  }

  function setAsks(list) {
    const known = new Set(S.asks.map(a => a.id));
    S.asks = list;
    for (const a of list) {
      if (known.has(a.id)) continue;
      const inst = S.byId.get(a.instance);
      UI.notify(`${instTitle(inst) || 'pai'} asks`, a.question, () => Sheets.ask(a));
      if (!UI.currentSheet() && (!a.instance || a.instance === S.current || document.hidden)) Sheets.ask(a);
    }
    if (UI.currentSheet() && UI.currentSheet().startsWith('ask:') &&
        !list.find(a => 'ask:' + a.id === UI.currentSheet())) UI.closeSheet(true);
    renderAlerts();
  }

  function promptArrived(p) {
    S.prompts.set(p.id, p);
    const recent = Date.now() - S.lastAction < 15000;
    UI.notify('Emacs asks', p.text);
    if (!UI.currentSheet() || (p.origin && recent)) Sheets.prompt(p);
    renderAlerts();
  }

  function promptClosed(id) {
    S.prompts.delete(id);
    if (UI.currentSheet() === 'prompt:' + id) UI.closeSheet(true);
    renderAlerts();
  }

  function renderAlerts() {
    const mk = forChat => {
      const out = [];
      for (const p of S.prompts.values()) {
        if (forChat && p.instance && p.instance !== S.current) continue;
        out.push(h('div', { class: 'alert', onclick: () => Sheets.prompt(p) },
                   h('b', {}, 'Emacs is asking' + (p.caller ? ` (${p.caller})` : '')), p.text));
      }
      for (const a of S.asks) {
        if (forChat && a.instance && a.instance !== S.current) continue;
        const inst = S.byId.get(a.instance);
        out.push(h('div', { class: 'alert', onclick: () => Sheets.ask(a) },
                   h('b', {}, `${instTitle(inst) || 'pai'} asks`), a.question));
      }
      return out;
    };
    $('#global-alerts').replaceChildren(...mk(false));
    $('#chat-alerts').replaceChildren(...mk(true));
    Chat.renderList();
    updateTitle();
  }

  function updateTitle() {
    const n = S.attention.size + S.prompts.size + S.asks.length;
    const cur = S.byId.get(S.current);
    document.title = (n ? `(${n}) ` : '') + (cur ? instTitle(cur) + ' · ' : '') + 'pai';
  }

  async function handle(events) {
    for (const e of events) {
      try {
        switch (e.t) {
          case 'instances': setInstances(e.list); break;
          case 'chrome': S.chrome[e.i] = e.chrome; if (e.i === S.current) Chat.renderChrome(); break;
          case 'item': Chat.onItem(e); break;
          case 'delta': Chat.onDelta(e); break;
          case 'reset': Chat.onReset(e); break;
          case 'prompt': promptArrived(e.prompt); break;
          case 'prompt-closed': promptClosed(e.id); break;
          case 'asks': setAsks(e.list); break;
          case 'buffers': S.buffers = e.list; break;
          case 'opened':
            if (Date.now() - S.lastAction < 10000) Buffer.open(e.buffer.b);
            break;
          case 'buffer-changed': if (e.b === S.buffer) Buffer.refresh(); break;
          case 'toast': UI.toast(e.text, e.level); break;
          case 'focus': openChat(e.i); break;
          case 'resync': await resync(); break;
        }
      } catch (err) { console.error('event', e, err); }
    }
  }

  function applyState(st) {
    S.levels = st.levels || [];
    S.colors = st.colors || {};
    if (S.colors.bg) document.documentElement.style.setProperty('--code-bg', S.colors.bg);
    if (S.colors.fg) document.documentElement.style.setProperty('--code-fg', S.colors.fg);
    S.buffers = st.buffers || [];
    S.prompts = new Map((st.prompts || []).map(p => [p.id, p]));
    setInstances(st.instances || []);
    setAsks(st.asks || []);
    renderAlerts();
  }

  async function resync() {
    applyState(await Net.get('/api/state'));
    if (S.current) Chat.open(S.current, true);
  }

  async function connect() {
    if (S.poller) S.poller.stop();
    const st = await Net.get('/api/hello');
    S.client = st.client;
    applyState(st);
    S.poller = Net.poller(S.client, handle, status => {
      $('#conn').className = 'conn ' + (status === 'on' ? 'on' : 'off');
      if (status === 'login') showLogin();
      if (status === 'gone') connect().catch(() => setTimeout(connect, 2000));
    });
    route();
    sendView();
  }

  function route() {
    const m = location.hash.match(/^#(i\d+)$/);
    if (m && S.byId.has(m[1])) openChat(m[1], true);
    else if (!matchMedia('(min-width: 900px)').matches) openList(true);
    else if (!S.current && S.instances.length) openChat(S.instances.find(i => !i.subagent)?.id || S.instances[0].id, true);
  }

  function openChat(id, fromRoute) {
    if (!S.byId.has(id)) return;
    S.attention.delete(id);
    document.body.classList.add('in-chat', 'has-chat');
    if (!fromRoute && location.hash !== '#' + id) history.pushState(null, '', '#' + id);
    Chat.open(id);
    sendView();
    renderAlerts();
  }

  function openList(fromRoute) {
    document.body.classList.remove('in-chat');
    if (!matchMedia('(min-width: 900px)').matches) {
      S.current = null;
      document.body.classList.remove('has-chat');
      sendView();
    }
    if (!fromRoute && location.hash) history.pushState(null, '', location.pathname);
    Chat.renderList();
    updateTitle();
  }

  function showLogin() {
    $('#login').classList.remove('hidden');
    $('#login-password').focus();
  }

  $('#login-form').addEventListener('submit', async e => {
    e.preventDefault();
    $('#login-error').textContent = '';
    try {
      await Net.post('/api/login', { password: $('#login-password').value });
      $('#login-password').value = '';
      $('#login').classList.add('hidden');
      await connect();
    } catch (err) { $('#login-error').textContent = err.message; }
  });

  window.addEventListener('popstate', () => {
    if (location.hash) route(); else openList(true);
  });
  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) {
      if (S.poller) S.poller.kick();
      if (S.current) S.attention.delete(S.current);
      updateTitle();
      Chat.renderList();
    }
  });

  async function boot() {
    try {
      const auth = await Net.get('/api/auth');
      if (auth.password && !auth.authed) { showLogin(); return; }
      await connect();
    } catch (e) {
      UI.toast('Cannot reach Emacs: ' + e.message, 'error');
      setTimeout(boot, 3000);
    }
  }

  return { act, boot, openChat, openList, instTitle, sendView, renderAlerts, updateTitle, connect };
})();

App.boot();
