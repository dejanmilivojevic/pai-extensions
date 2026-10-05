// A remote Emacs buffer: live text with faces, taps on buttons/fields/text,
// a key bar (with sticky Ctrl/Meta and prefix keys like C-c) and typing.
'use strict';

const Buffer = (() => {
  const view = $('#buffer-view');
  let seq = [], ctrl = false, meta = false, refreshTimer = null, loading = false, again = false;
  const KEYS = ['RET', 'TAB', 'S-TAB', 'C-g', 'C-c C-c', 'C-c C-k', 'q', 'n', 'p', 'SPC', 'DEL',
                '<up>', '<down>', '<left>', '<right>', 'C-a', 'C-e', 'M-<', 'M->', 'g', 'o', '?'];

  function build() {
    const pre = h('pre', { class: 'buf' });
    pre.addEventListener('click', onTap);
    const text = h('input', { placeholder: 'Type here (each key runs as in Emacs)', autocapitalize: 'off',
                              autocomplete: 'off', spellcheck: 'false', enterkeyhint: 'send' });
    const sendText = () => {
      if (!text.value) { key('RET'); return; }
      type(text.value); text.value = '';
    };
    text.addEventListener('keydown', e => { if (e.key === 'Enter') { e.preventDefault(); sendText(); } });
    const keys = h('div', { class: 'keys' },
      h('button', { class: 'mod-c', onclick: () => { ctrl = !ctrl; mods(); } }, 'Ctrl'),
      h('button', { class: 'mod-m', onclick: () => { meta = !meta; mods(); } }, 'Meta'),
      ...KEYS.map(k => h('button', { onclick: () => key(k) }, k)));
    view.replaceChildren(
      h('header', { class: 'bar' },
        h('button', { class: 'icon-btn', 'aria-label': 'Close', onclick: close }, '‹'),
        h('div', { class: 'bar-title chat-title' }, h('div', { class: 'name' }), h('div', { class: 'sub' })),
        h('button', { class: 'icon-btn', title: 'Refresh', onclick: refresh }, '⟳'),
        h('button', { class: 'icon-btn', title: 'Kill buffer', onclick: () => Sheets.confirm('Kill this buffer in Emacs?', () => {
          App.act({ a: 'kill-buffer', b: S.buffer }); close();
        }) }, '🗑')),
      pre, h('div', { class: 'keyseq' }), keys,
      h('div', { class: 'buf-input' }, text, h('button', { onclick: sendText }, '⏎')));
  }

  function mods() {
    view.querySelector('.mod-c').classList.toggle('on', ctrl);
    view.querySelector('.mod-m').classList.toggle('on', meta);
    view.querySelector('.keyseq').textContent = seq.length ? seq.join(' ') + ' …' : '';
  }

  function withMods(k) {
    if (!ctrl && !meta) return k;
    const out = (ctrl ? 'C-' : '') + (meta ? 'M-' : '') + k;
    ctrl = meta = false;
    return out;
  }

  async function key(k) {
    const full = [...seq, withMods(k)].join(' ');
    try {
      const r = await Net.get('/api/keykind', { b: S.buffer, keys: full });
      if (r.kind === 'prefix') { seq = full.split(' '); mods(); return; }
      seq = []; mods();
      if (r.kind === 'undefined') { UI.toast(`${full} is undefined here`, 'warning'); return; }
      await App.act({ a: 'key', b: S.buffer, keys: full });
      soon();
    } catch (e) { seq = []; mods(); UI.toast(e.message, 'error'); }
  }

  async function type(text) {
    if (ctrl || meta || seq.length) {      // a modified key or a prefix waiting: send as keys
      for (const ch of text) await key(ch === ' ' ? 'SPC' : ch);
      return;
    }
    await App.act({ a: 'type', b: S.buffer, text });
    soon();
  }

  function posAt(e) {
    const span = e.target.closest('[data-p]');
    if (!span) return null;
    let offset = 0;
    const r = document.caretRangeFromPoint ? document.caretRangeFromPoint(e.clientX, e.clientY)
      : (document.caretPositionFromPoint && (() => {
          const c = document.caretPositionFromPoint(e.clientX, e.clientY);
          return c && { startContainer: c.offsetNode, startOffset: c.offset };
        })());
    if (r && span.contains(r.startContainer)) offset = [...r.startContainer.textContent.slice(0, r.startOffset)].length;
    return { span, pos: +span.dataset.p + offset };
  }

  async function onTap(e) {
    const hit = posAt(e);
    if (!hit) return;
    const cls = hit.span.className;
    if (cls === 'field') { editField(+hit.span.dataset.p); return; }
    await App.act({ a: cls === 'button' ? 'click' : 'goto', b: S.buffer, p: hit.pos });
    soon();
  }

  async function editField(pos) {
    try {
      const f = await Net.get('/api/field', { b: S.buffer, p: pos });
      if (f.error) { UI.toast(f.error, 'warning'); return; }
      UI.openSheet('field', sheet => {
        const field = h('textarea', { rows: 2 });
        field.value = (f.value || '').replace(/\s+$/, '');
        const ok = () => { UI.closeSheet(true); App.act({ a: 'field', b: S.buffer, p: pos, value: field.value }); soon(); };
        field.addEventListener('keydown', e => { if (e.key === 'Enter' && !e.shiftKey) { e.preventDefault(); ok(); } });
        sheet.append(h('h3', {}, 'Edit field'), field, h('div', { class: 'btns' },
          h('button', { onclick: () => UI.closeSheet() }, 'Cancel'),
          h('button', { class: 'primary', onclick: ok }, 'Set')));
      });
    } catch (err) { UI.toast(err.message, 'error'); }
  }

  function soon() { clearTimeout(refreshTimer); refreshTimer = setTimeout(refresh, 150); }

  async function refresh() {
    if (!S.buffer) return;
    if (loading) { again = true; return; }
    loading = true;
    try {
      const r = await Net.get('/api/buffer', { b: S.buffer });
      const pre = view.querySelector('pre.buf');
      const atEnd = pre.scrollHeight - pre.scrollTop - pre.clientHeight < 40;
      const top = pre.scrollTop;
      view.querySelector('.name').textContent = r.name;
      view.querySelector('.sub').textContent = r.mode + (r.readonly ? ' · read-only' : '');
      if (r.colors) { pre.style.background = r.colors.bg || ''; pre.style.color = r.colors.fg || ''; }
      pre.innerHTML = r.html;
      if (atEnd && r.point >= r.size) pre.scrollTop = pre.scrollHeight;
      else pre.scrollTop = top;
      if (pre.dataset.b !== r.b) {
        pre.dataset.b = r.b;
        const pt = pre.querySelector('.pt');
        if (pt) pt.scrollIntoView({ block: 'center' });
      }
    } catch (e) {
      UI.toast(e.message, 'error');
      if (e.status === 400) close();
    } finally {
      loading = false;
      if (again) { again = false; refresh(); }
    }
  }

  function open(b) {
    if (!view.firstChild) build();
    if (S.buffer !== b) { view.querySelector('pre.buf').innerHTML = ''; delete view.querySelector('pre.buf').dataset.b; }
    S.buffer = b;
    seq = []; ctrl = meta = false; mods();
    view.classList.remove('hidden');
    App.sendView();
    refresh();
  }

  function close() {
    S.buffer = null;
    view.classList.add('hidden');
    App.sendView();
  }

  return { open, close, refresh };
})();
