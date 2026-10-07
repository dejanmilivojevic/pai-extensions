// The instance list and the chat view: transcript, header, composer.
'use strict';

const Chat = (() => {
  const logs = new Map();       // instance id -> { g, items: Map, order: [], more }
  let renderQueued = new Set(), rafPending = false;
  let completion = null, completeTimer = null, completeSeq = 0;
  const pending = [];           // images to attach: { data, mime, url }

  // ---------- list ----------

  function renderList() {
    const box = $('#instances');
    const kids = new Map();
    const tops = [];
    for (const inst of S.instances) {
      if (inst.parent && S.byId.has(inst.parent)) {
        if (!kids.has(inst.parent)) kids.set(inst.parent, []);
        kids.get(inst.parent).push(inst);
      } else tops.push(inst);
    }
    const out = [];
    const add = (inst, depth) => {
      out.push(card(inst, depth));
      for (const k of kids.get(inst.id) || []) add(k, depth + 1);
    };
    tops.forEach(i => add(i, 0));
    if (!out.length) out.push(h('div', { class: 'empty' }, 'No pai instances. Tap ＋ to start one.'));
    box.replaceChildren(...out);
  }

  function card(inst, depth) {
    const busy = inst.active || inst.compacting;
    const prompts = [...S.prompts.values()].filter(p => p.instance === inst.id).length;
    const pct = Math.min(100, inst.pct || 0);
    return h('button', {
      class: `inst${depth ? ' child' : ''}${inst.id === S.current ? ' sel' : ''}`,
      style: depth > 1 ? `margin-left:${18 * depth}px;width:calc(100% - ${18 * depth}px)` : null,
      onclick: () => App.openChat(inst.id),
    },
      h('div', { class: 'row' },
        h('span', { class: `dot ${busy ? 'busy' : 'idle'}` }),
        h('span', { class: 't' }, (inst.subagent ? '↳ ' : '') + App.instTitle(inst)),
        inst.asks ? h('span', { class: 'badge' }, `? ${inst.asks}`) : null,
        prompts ? h('span', { class: 'badge' }, `⌨ ${prompts}`) : null,
        S.attention.has(inst.id) ? h('span', { class: 'badge info' }, 'done') : null,
        h('span', { class: 'm' }, inst.cost)),
      h('div', { class: 'm' }, `${inst.cwd} · ${inst.model || 'no model'} · ${inst.thinking}`),
      h('div', { class: 'm' }, busy ? inst.status : `ctx ${UI.fmtTokens(inst.ctx)} (${inst.pct}%)`
        + (inst.queued ? ` · ${inst.queued} queued` : '')),
      h('div', { class: 'meter' }, h('i', { class: pct > 90 ? 'full' : pct > 70 ? 'hot' : '', style: `width:${pct}%` })));
  }

  // ---------- header ----------

  function renderHeader() {
    const inst = S.byId.get(S.current);
    if (!inst) return;
    $('#chat-name').textContent = App.instTitle(inst);
    $('#chat-sub').textContent = inst.cwd + (inst.subagent ? ' · subagent' : '');
    const busy = inst.active || inst.compacting;
    const pill = $('#chat-status');
    pill.textContent = busy ? '● ' + inst.status : inst.status;
    pill.className = 'status-pill' + (busy ? ' busy' : '');
    $('#stop-btn').classList.toggle('hidden', !busy);
    const chips = [
      h('button', { class: 'chip', onclick: () => Sheets.model(inst.id) }, '◆ ' + (inst.model || 'model?')),
      h('button', { class: 'chip', onclick: () => Sheets.thinking(inst.id) }, '✦ ' + inst.thinking),
      h('span', { class: 'chip' }, `ctx ${UI.fmtTokens(inst.ctx)}/${UI.fmtTokens(inst.window)} (${inst.pct}%)`),
      h('span', { class: 'chip' }, inst.cost),
    ];
    if (inst.queued) chips.push(h('span', { class: 'chip' }, `${inst.queued} queued`));
    const ch = S.chrome[inst.id];
    if (ch) {
      if (ch.usage) chips.push(h('span', { class: 'chip', html: ch.usage }));
      if (ch.header) chips.push(h('span', { class: 'chip', html: ch.header }));
      for (const s of ch.statuses || []) chips.push(h('span', { class: 'chip', html: s }));
    }
    $('#chips').replaceChildren(...chips);
  }

  function renderChrome() {
    renderHeader();
    const ch = S.chrome[S.current];
    const above = $('#above');
    if (!ch) { above.replaceChildren(); return; }
    const html = [ch.above, ch.footer].filter(x => x && x.trim()).join('\n');
    if (above.dataset.html !== html) { above.dataset.html = html; above.innerHTML = html; }
    above.classList.toggle('collapsed', !!UI.prefs.aboveCollapsed);
  }
  $('#above').addEventListener('click', () => {
    UI.prefs.aboveCollapsed = !UI.prefs.aboveCollapsed; UI.savePrefs(); renderChrome();
  });

  // ---------- transcript ----------

  async function open(id, force) {
    const changed = S.current !== id;
    S.current = id;
    renderHeader();
    renderList();
    if (changed) { $('#transcript').replaceChildren(); $('#above').replaceChildren(); delete $('#above').dataset.html; }
    Net.get('/api/chrome', { i: id }).then(ch => { S.chrome[id] = ch; if (S.current === id) renderChrome(); })
      .catch(() => {});
    if (changed || force || !logs.has(id)) await load(id);
    if (!matchMedia('(pointer: coarse)').matches) $('#input').focus();
  }

  async function load(id) {
    try {
      const snap = await Net.get('/api/transcript', { i: id });
      if (S.current !== id) return;
      const log = { g: snap.g, items: new Map(), order: [], more: snap.more };
      for (const it of snap.items) { log.items.set(it.id, it); log.order.push(it.id); }
      logs.set(id, log);
      renderAll(log);
      scrollEnd();
    } catch (e) { UI.toast(e.message, 'error'); }
  }

  async function loadMore() {
    const id = S.current, log = logs.get(id);
    if (!log || !log.order.length) return;
    const box = $('#transcript');
    const before = box.scrollHeight - box.scrollTop;
    const snap = await Net.get('/api/transcript', { i: id, before: log.order[0] });
    if (snap.g !== log.g || S.current !== id) return;
    log.order = snap.items.map(it => it.id).concat(log.order);
    for (const it of snap.items) log.items.set(it.id, it);
    log.more = snap.more;
    renderAll(log);
    box.scrollTop = box.scrollHeight - before;
  }

  function renderAll(log) {
    const box = $('#transcript');
    const els = [];
    if (log.more) els.push(h('button', { class: 'more', onclick: loadMore }, 'Earlier messages'));
    for (const id of log.order) els.push(renderItem(log.items.get(id)));
    box.replaceChildren(...els);
  }

  function nearEnd() {
    const b = $('#transcript');
    return b.scrollHeight - b.scrollTop - b.clientHeight < 120;
  }
  function scrollEnd() { const b = $('#transcript'); b.scrollTop = b.scrollHeight; $('#jump-btn').classList.add('hidden'); }
  $('#transcript').addEventListener('scroll', () => $('#jump-btn').classList.toggle('hidden', nearEnd()));
  $('#jump-btn').addEventListener('click', scrollEnd);

  // a collapsible block (not <details>: WebKitGTK draws closed ones badly)
  function fold(cls, head, body) {
    const b = h('div', { class: 'fold-body' }, body);
    const el = h('div', { class: 'fold ' + cls },
                 h('div', { class: 'fold-head', role: 'button', tabindex: '0' }, head), b);
    el.firstChild.addEventListener('click', () => el.classList.toggle('open'));
    return el;
  }
  const setKids = (el, ...kids) => el.replaceChildren(...kids.filter(k => k != null && k !== false));

  function summarizeArgs(args) {
    try { return JSON.stringify(JSON.parse(args)); } catch (_) { return (args || '').replace(/\s+/g, ' '); }
  }

  function renderItem(it) {
    let el;
    switch (it.kind) {
      case 'user':
        el = h('div', { class: 'msg user' }, it.text,
               it.images ? h('div', { class: 'att' }, `[${it.images} image${it.images > 1 ? 's' : ''}]`) : null);
        break;
      case 'note':
        el = h('div', { class: 'msg note' + (/error/.test(it.face) ? ' err' : ''), html: it.html || MD.esc(it.text) });
        break;
      case 'assistant': {
        el = h('div', { class: 'msg assistant' });
        const blocks = it.blocks || [];
        let fenceBase = 0;
        blocks.forEach((b, i) => {
          if (b.type === 'thinking') {
            el.append(fold('think', h('span', {}, 'thinking'), h('pre', {}, b.text)));
          } else {
            const streaming = it.streaming && i === blocks.length - 1;
            const fences = (it.fences || []).slice(fenceBase);
            fenceBase += MD.countFences(b.text);
            el.append(h('div', { class: 'md' + (streaming ? ' streaming' : ''), html: MD.render(b.text, fences) }));
          }
        });
        if (it.streaming && !blocks.length) el.append(h('div', { class: 'md streaming' }));
        break;
      }
      case 'tool': {
        const st = it.status;
        const icon = st === 'running' ? '…' : st === 'error' ? '✗' : '✓';
        const body = h('div', { class: 'tb' });
        el = fold('msg tool',
                  [h('span', { class: 'tn' }, '⚙ ' + it.name),
                   h('span', { class: 'ta' }, summarizeArgs(it.args)),
                   h('span', { class: `ts ${st}` }, icon)],
                  body);
        const fill = (args, result, html) => {
          setKids(body,
            h('h6', {}, 'arguments'), h('pre', {}, args || '{}'),
            st === 'running' ? null : h('h6', {}, st === 'error' ? 'error' : 'result'),
            st === 'running' ? null
              : html ? h('pre', { html }) : h('pre', { class: st === 'error' ? 'err' : '' }, result || ''),
            (it.truncated || it.argsTruncated) ? h('button', {
              onclick: async ev => {
                ev.target.disabled = true;
                try {
                  const d = await Net.get('/api/tool', { i: S.current, id: it.id });
                  fill(d.args, d.result, null);
                } catch (e) { UI.toast(e.message, 'error'); }
              } }, 'Show everything') : null);
        };
        fill(it.args, it.result, it.html);
        if (st === 'error') el.classList.add('open');
        break;
      }
      default:
        el = h('div', { class: 'msg note' }, JSON.stringify(it));
    }
    el.dataset.id = it.id;
    return el;
  }

  function replaceItem(log, it) {
    const box = $('#transcript');
    const old = box.querySelector(`[data-id="${it.id}"]`);
    const keepOpen = old && old.classList.contains('open');
    const el = renderItem(it);
    if (keepOpen) el.classList.add('open');
    if (old) old.replaceWith(el); else box.append(el);
  }

  function flushRenders() {
    if (!rafPending) return;
    rafPending = false;
    const log = logs.get(S.current);
    if (!log) { renderQueued.clear(); return; }
    const stick = nearEnd();
    for (const id of renderQueued) { const it = log.items.get(id); if (it) replaceItem(log, it); }
    renderQueued.clear();
    if (stick) scrollEnd();
  }
  function queueRender(id) {
    renderQueued.add(id);
    if (!rafPending) {
      rafPending = true;
      // animation frames pause in a hidden page; the timeout still renders
      requestAnimationFrame(flushRenders);
      setTimeout(flushRenders, 150);
    }
  }

  function onItem(e) {
    const log = logs.get(e.i);
    if (!log) return;
    if (log.g !== e.g) { if (e.i === S.current) load(e.i); else logs.delete(e.i); return; }
    if (!log.items.has(e.item.id)) log.order.push(e.item.id);
    log.items.set(e.item.id, e.item);
    if (e.i === S.current) queueRender(e.item.id);
  }

  function onDelta(e) {
    const log = logs.get(e.i);
    if (!log || log.g !== e.g) return;
    const it = log.items.get(e.id);
    if (!it) return;
    it.blocks = it.blocks || [];
    const b = it.blocks[e.b];
    if (b && b.type === e.k) b.text += e.s; else it.blocks[e.b] = { type: e.k, text: e.s };
    it.streaming = true;
    if (e.i === S.current) queueRender(e.id);
  }

  function onReset(e) {
    if (e.i === S.current) load(e.i); else logs.delete(e.i);
  }

  // ---------- composer ----------

  const input = $('#input');
  const coarse = () => matchMedia('(pointer: coarse)').matches;

  function grow() { input.style.height = 'auto'; input.style.height = Math.min(input.scrollHeight, innerHeight * 0.4) + 'px'; }

  async function send() {
    const text = input.value;
    if (!text.trim() && !pending.length) return;
    if (!S.current) return;
    const images = pending.map(p => ({ data: p.data, mime: p.mime }));
    input.value = ''; grow(); hideCompletion();
    pending.length = 0; renderAttachments();
    await App.act({ a: 'send', i: S.current, text, images });
    scrollEnd();
  }

  $('#composer').addEventListener('submit', e => { e.preventDefault(); send(); });
  $('#stop-btn').addEventListener('click', () => App.act({ a: 'interrupt', i: S.current }));
  $('#back-btn').addEventListener('click', () => App.openList());
  $('#chat-menu-btn').addEventListener('click', () => Sheets.chatMenu(S.current));

  input.addEventListener('input', () => { grow(); scheduleCompletion(); });
  input.addEventListener('keydown', e => {
    // Ctrl+Enter (Cmd+Enter on macOS) always sends, also on touch devices
    // where a plain Enter inserts a newline
    if (e.key === 'Enter' && (e.ctrlKey || e.metaKey) && !e.isComposing) {
      e.preventDefault(); hideCompletion(); send(); return;
    }
    if (completion && !completion.box.classList.contains('hidden')) {
      const n = completion.items.length;
      if (e.key === 'ArrowDown') { e.preventDefault(); selectCompletion((completion.sel + 1) % n); return; }
      if (e.key === 'ArrowUp') { e.preventDefault(); selectCompletion((completion.sel - 1 + n) % n); return; }
      if (e.key === 'Tab' || (e.key === 'Enter' && completion.sel >= 0)) {
        e.preventDefault(); accept(completion.items[Math.max(0, completion.sel)]); return;
      }
      if (e.key === 'Escape') { hideCompletion(); return; }
    } else if (e.key === 'Tab' && !e.shiftKey) {
      e.preventDefault(); requestCompletion(true); return;
    }
    if (e.key === 'Enter' && !e.shiftKey && !coarse()) { e.preventDefault(); send(); }
  });

  function wantsCompletion() {
    const pos = input.selectionStart, text = input.value.slice(0, pos);
    if (/^\/\S*(\s|$)/.test(input.value) && !/\n/.test(text)) return true;
    return /(^|\s)[@*][^\s]*$/.test(text);
  }

  function scheduleCompletion() {
    clearTimeout(completeTimer);
    if (!wantsCompletion()) { hideCompletion(); return; }
    completeTimer = setTimeout(() => requestCompletion(false), 120);
  }

  async function requestCompletion(explicit) {
    if (!S.current) return;
    const seq = ++completeSeq;
    const text = input.value, pos = input.selectionStart;
    try {
      const r = await Net.post('/api/complete', { i: S.current, text, pos });
      if (seq !== completeSeq) return;
      if (!r.items || !r.items.length) { hideCompletion(); if (explicit) UI.toast('No completions'); return; }
      showCompletion(r, text);
    } catch (_) { hideCompletion(); }
  }

  function showCompletion(r, text) {
    const box = $('#complete');
    completion = { box, items: r.items, beg: r.beg, end: r.end, text, sel: coarse() ? -1 : 0 };
    box.replaceChildren(...r.items.map((it, i) => h('button', {
      type: 'button', class: i === completion.sel ? 'sel' : '',
      onmousedown: e => e.preventDefault(),
      onclick: () => accept(it),
    }, h('span', { class: 'v' }, it.v), h('span', { class: 'a' }, it.a || ''))));
    box.classList.remove('hidden');
  }

  function selectCompletion(i) {
    completion.sel = i;
    [...completion.box.children].forEach((b, j) => b.classList.toggle('sel', j === i));
    completion.box.children[i]?.scrollIntoView({ block: 'nearest' });
  }

  function hideCompletion() { $('#complete').classList.add('hidden'); completion = null; }

  function accept(it) {
    if (!completion) return;
    const { beg, end } = completion;
    const v = input.value;
    const more = !/[/]$/.test(it.v);
    input.value = v.slice(0, beg) + it.v + (more ? ' ' : '') + v.slice(end).replace(/^ /, '');
    const caret = beg + it.v.length + (more ? 1 : 0);
    input.setSelectionRange(caret, caret);
    hideCompletion();
    grow();
    input.focus();
    // like Emacs: go on to the next argument when the command has one
    if (input.value.startsWith('/')) requestCompletion(false);
    else if (!more) requestCompletion(false);
  }

  // ---------- attachments ----------

  $('#attach-btn').addEventListener('click', () => $('#file-input').click());
  $('#file-input').addEventListener('change', async e => {
    for (const f of e.target.files) await attach(f);
    e.target.value = '';
  });
  input.addEventListener('paste', async e => {
    const files = [...(e.clipboardData?.files || [])];
    if (files.length) { e.preventDefault(); for (const f of files) await attach(f); }
  });

  async function attach(file) {
    if (!S.current) return;
    if (/^image\/(png|jpeg|gif|webp)$/.test(file.type)) {
      try { pending.push(await shrinkImage(file)); renderAttachments(); }
      catch (e) { UI.toast('Could not read the image: ' + e.message, 'error'); }
      return;
    }
    try {
      UI.toast(`Uploading ${file.name}…`);
      const r = await Net.post(`/api/upload?i=${S.current}&name=${encodeURIComponent(file.name)}`,
                               await file.arrayBuffer(), true);
      insertAtCaret(r.mention + ' ');
      UI.toast(`Saved as ${r.path}`);
    } catch (e) { UI.toast('Upload failed: ' + e.message, 'error'); }
  }

  function insertAtCaret(s) {
    const p = input.selectionStart ?? input.value.length;
    const pre = input.value.slice(0, p);
    const sep = pre && !/\s$/.test(pre) ? ' ' : '';
    input.value = pre + sep + s + input.value.slice(p);
    grow();
  }

  // Images are scaled to at most 1568 px (what pai sends models) in the
  // browser, so Emacs never has to resize them.
  function shrinkImage(file) {
    return new Promise((resolve, reject) => {
      const url = URL.createObjectURL(file);
      const img = new Image();
      img.onload = () => {
        const max = 1568, scale = Math.min(1, max / Math.max(img.width, img.height));
        const keep = scale === 1 && file.type !== 'image/webp' && file.size < 4e6;
        const finish = (blob, mime) => {
          const fr = new FileReader();
          fr.onload = () => resolve({ data: fr.result.split(',')[1], mime, url });
          fr.onerror = () => reject(fr.error);
          fr.readAsDataURL(blob);
        };
        if (keep) { finish(file, file.type); return; }
        const c = document.createElement('canvas');
        c.width = Math.round(img.width * scale); c.height = Math.round(img.height * scale);
        c.getContext('2d').drawImage(img, 0, 0, c.width, c.height);
        const mime = file.type === 'image/png' ? 'image/png' : 'image/jpeg';
        c.toBlob(b => b ? finish(b, mime) : reject(new Error('encode failed')), mime, 0.87);
      };
      img.onerror = () => reject(new Error('not an image'));
      img.src = url;
    });
  }

  function renderAttachments() {
    $('#attachments').replaceChildren(...pending.map((p, i) => h('div', { class: 'att-chip' },
      h('img', { src: p.url, alt: '' }),
      h('button', { type: 'button', onclick: () => { pending.splice(i, 1); renderAttachments(); } }, '×'))));
  }

  return { renderList, renderHeader, renderChrome, open, onItem, onDelta, onReset, insertAtCaret };
})();
