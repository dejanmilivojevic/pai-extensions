// Bottom sheets: Emacs prompts, ask_user questions, pickers, menus.
'use strict';

const Sheets = (() => {
  function from(instanceId, caller) {
    const inst = S.byId.get(instanceId);
    return h('div', { class: 'from' }, inst ? App.instTitle(inst) : (caller || 'Emacs'));
  }

  // filter like orderless: every space-separated word must occur
  function matches(c, q) {
    const s = c.toLowerCase();
    return q.toLowerCase().split(/\s+/).filter(Boolean).every(w => s.includes(w));
  }

  // ---------- minibuffer prompts ----------

  function prompt(p) {
    let answered = false;
    const answer = value => {
      answered = true;
      App.act({ a: 'prompt', id: p.id, value });
      UI.closeSheet(true);
    };
    const cancel = () => { if (!answered) App.act({ a: 'prompt', id: p.id, cancel: true }); };
    UI.openSheet('prompt:' + p.id, sheet => {
      sheet.append(from(p.instance, p.caller), h('h3', { class: 'q' }, p.text.trim()));
      const btns = h('div', { class: 'btns' });
      if (p.kind === 'y-or-n' || p.kind === 'yes-or-no') {
        const yes = p.kind === 'y-or-n' ? 'y' : 'yes', no = p.kind === 'y-or-n' ? 'n' : 'no';
        btns.append(h('button', { onclick: () => UI.closeSheet() }, 'Cancel'),
                    h('button', { onclick: () => answer(no) }, 'No'),
                    h('button', { class: 'primary', onclick: () => answer(yes) }, 'Yes'));
        sheet.append(btns);
        return;
      }
      const field = h('input', { type: p.kind === 'password' ? 'password' : 'text',
                                 placeholder: p.default ? `default: ${p.default}` : '',
                                 autocomplete: 'off', autocapitalize: 'off', spellcheck: 'false' });
      field.value = p.initial || '';
      sheet.append(field);
      let list = null;
      if (p.kind === 'completion') {
        list = h('div', { class: 'list' });
        sheet.append(list);
        // a dynamic table (file names) completes the input after `base',
        // as Emacs does: candidates are the last path component
        let cands = p.candidates || [], base = p.base || '', seq = 0;
        const pick = c => {
          if (p.dynamic && /\/$/.test(c)) { field.value = base + c; refresh(); field.focus(); }
          else answer(p.dynamic ? base + c : c);
        };
        const show = () => {
          const q = p.dynamic ? '' : field.value;
          const shown = cands.filter(c => matches(c, q)).slice(0, 300);
          list.replaceChildren(...shown.map(c => h('button', { onclick: () => pick(c) }, c)));
        };
        const refresh = async () => {
          if (!p.dynamic) { show(); return; }
          const my = ++seq;
          try {
            const r = await Net.post('/api/prompt-complete', { id: p.id, input: field.value });
            if (my === seq) { cands = r.candidates || []; base = r.base || ''; show(); }
          } catch (_) { /* closed meanwhile */ }
        };
        // TAB like the minibuffer: complete a sole match, else the common prefix
        const tab = () => {
          const shown = p.dynamic ? cands : cands.filter(c => matches(c, field.value));
          if (!shown.length) { UI.toast('No match'); return; }
          if (!p.dynamic) { if (shown.length === 1) field.value = shown[0]; return; }
          let pre = shown[0];
          for (const c of shown) { let i = 0; while (i < pre.length && i < c.length && pre[i] === c[i]) i++; pre = pre.slice(0, i); }
          const tail = field.value.slice(base.length);
          if (shown.length === 1 || pre.length > tail.length) {
            field.value = base + (shown.length === 1 ? shown[0] : pre);
            refresh();
          }
        };
        field.addEventListener('keydown', e => { if (e.key === 'Tab') { e.preventDefault(); tab(); } });
        field.addEventListener('input', refresh);
        show();
      }
      const ok = () => {
        const v = field.value;
        if (p.kind === 'completion' && p.require && v && !(p.candidates || []).includes(v) && !p.dynamic) {
          const hit = (p.candidates || []).filter(c => matches(c, v));
          if (hit.length === 1) { answer(hit[0]); return; }
          UI.toast('Pick one of the choices', 'warning'); return;
        }
        answer(v);
      };
      field.addEventListener('keydown', e => { if (e.key === 'Enter') { e.preventDefault(); ok(); } });
      btns.append(h('button', { onclick: () => UI.closeSheet() }, 'Cancel'),
                  h('button', { class: 'primary', onclick: ok }, 'OK'));
      sheet.append(btns);
    }, cancel);
  }

  // ---------- ask_user_question ----------

  function ask(a) {
    const chosen = new Set();
    UI.openSheet('ask:' + a.id, sheet => {
      sheet.append(from(a.instance), h('h3', { class: 'q' }, a.question));
      if (a.details) sheet.append(h('div', { class: 'md', html: MD.render(a.details) }));
      const other = h('textarea', { rows: a.mode === 'text' ? 4 : 2,
                                    placeholder: a.mode === 'text' ? 'Your answer' : `${a.other}: write your own answer` });
      const submit = (choices) => {
        App.act({ a: 'ask', id: a.id, choices, other: other.value, text: other.value });
        UI.closeSheet(true);
      };
      if (a.mode !== 'text') {
        const opts = h('div', { class: 'opts' });
        a.options.forEach((o, i) => {
          const b = h('button', { class: 'opt', onclick: () => {
            if (a.mode === 'single-select') { submit([i + 1]); return; }
            if (chosen.has(i + 1)) chosen.delete(i + 1); else chosen.add(i + 1);
            b.classList.toggle('on', chosen.has(i + 1));
          } }, h('span', {}, `${i + 1}. ${o.label}`), o.description ? h('small', {}, o.description) : null);
          opts.append(b);
        });
        sheet.append(opts);
      }
      sheet.append(other);
      sheet.append(h('div', { class: 'btns' },
        h('button', { class: 'danger', onclick: () => {
          App.act({ a: 'ask', id: a.id, cancel: true }); UI.closeSheet(true);
        } }, 'Cancel question'),
        h('button', { onclick: () => UI.closeSheet() }, 'Later'),
        h('button', { class: 'primary', onclick: () => {
          if (a.mode === 'text' && !other.value.trim()) { UI.toast('Write an answer', 'warning'); return; }
          if (a.mode === 'single-select' && !other.value.trim()) { UI.toast('Tap an option or write an answer', 'warning'); return; }
          if (a.mode === 'multi-select' && !chosen.size && !other.value.trim()) { UI.toast('Choose something', 'warning'); return; }
          submit([...chosen].sort((x, y) => x - y));
        } }, 'Submit')));
    });
  }

  // ---------- pickers ----------

  function pickList(key, title, items, current, onPick, filterable) {
    UI.openSheet(key, sheet => {
      sheet.append(h('h3', {}, title));
      const list = h('div', { class: 'list' });
      const field = filterable ? h('input', { placeholder: 'Filter', autocapitalize: 'off', spellcheck: 'false' }) : null;
      const show = () => list.replaceChildren(...items
        .filter(x => !field || matches(x, field.value))
        .map(x => h('button', { class: x === current ? 'cur' : '', onclick: () => { UI.closeSheet(true); onPick(x); } }, x)));
      if (field) {
        field.addEventListener('input', show);
        field.addEventListener('keydown', e => {
          if (e.key === 'Enter') { const first = list.querySelector('button'); if (first) first.click(); }
        });
        sheet.append(field);
      }
      show();
      sheet.append(list, h('div', { class: 'btns' }, h('button', { onclick: () => UI.closeSheet() }, 'Close')));
    });
  }

  async function model(i) {
    try {
      const m = await Net.get('/api/models', { i });
      pickList('model', 'Model', m.models, m.current, id => App.act({ a: 'model', i, id }), true);
    } catch (e) { UI.toast(e.message, 'error'); }
  }

  function thinking(i) {
    const inst = S.byId.get(i);
    pickList('thinking', 'Thinking level', S.levels, inst && inst.thinking,
             level => App.act({ a: 'thinking', i, level }));
  }

  async function newInstance() {
    let dirs = [];
    try { dirs = (await Net.get('/api/dirs')).dirs; } catch (_) { /* none */ }
    UI.openSheet('new', sheet => {
      sheet.append(h('h3', {}, 'New pai instance'));
      const field = h('input', { placeholder: 'Project directory, e.g. ~/prj/app', autocapitalize: 'off', spellcheck: 'false' });
      const go = dir => { UI.closeSheet(true); App.act({ a: 'new', dir }); UI.toast('Starting a new instance…'); };
      field.addEventListener('keydown', e => { if (e.key === 'Enter' && field.value.trim()) go(field.value.trim()); });
      sheet.append(field,
        h('div', { class: 'list' }, ...dirs.map(d => h('button', { onclick: () => go(d) }, d))),
        h('div', { class: 'btns' }, h('button', { onclick: () => UI.closeSheet() }, 'Cancel'),
          h('button', { class: 'primary', onclick: () => field.value.trim() && go(field.value.trim()) }, 'Start')));
    });
  }

  function chatMenu(i) {
    const inst = S.byId.get(i);
    if (!inst) return;
    const cmd = text => () => { UI.closeSheet(true); App.act({ a: 'send', i, text }); };
    const fill = text => () => {
      UI.closeSheet(true);
      const input = $('#input'); input.value = text; input.focus();
      input.dispatchEvent(new Event('input'));
    };
    UI.openSheet('menu', sheet => {
      sheet.append(h('h3', {}, App.instTitle(inst)), h('div', { class: 'from' }, inst.cwd),
        h('div', { class: 'list' },
          h('button', { onclick: () => { UI.closeSheet(true); model(i); } }, '◆ Model…'),
          h('button', { onclick: () => { UI.closeSheet(true); thinking(i); } }, '✦ Thinking level…'),
          h('button', { onclick: fill('/') }, '/ Slash command…'),
          h('button', { onclick: cmd('/resume') }, '↺ Resume a session…'),
          h('button', { onclick: cmd('/tree') }, '⑂ Go to a point of this session…'),
          h('button', { onclick: cmd('/new') }, '✚ Fresh session in this instance'),
          h('button', { onclick: cmd('/compact') }, '⇊ Compact the context'),
          h('button', { onclick: fill('/name ') }, '✎ Rename…'),
          h('button', { onclick: cmd('/menu') }, '⚙ Settings (/menu)'),
          h('button', { onclick: () => { UI.closeSheet(true); buffers(); } }, '▤ Emacs buffers…'),
          h('button', { class: 'danger', onclick: () => confirm(`Close ${App.instTitle(inst)}?`, () => {
            App.act({ a: 'close', i }); App.openList();
          }) }, '✕ Close this instance')),
        h('div', { class: 'btns' }, h('button', { onclick: () => UI.closeSheet() }, 'Close')));
    });
  }

  function confirm(text, onYes) {
    UI.openSheet('confirm', sheet => {
      sheet.append(h('h3', {}, text), h('div', { class: 'btns' },
        h('button', { onclick: () => UI.closeSheet() }, 'Cancel'),
        h('button', { class: 'primary', onclick: () => { UI.closeSheet(true); onYes(); } }, 'Yes')));
    });
  }

  async function buffers() {
    try { S.buffers = (await Net.get('/api/buffers')).list; } catch (_) { /* keep */ }
    UI.openSheet('buffers', sheet => {
      sheet.append(h('h3', {}, 'Emacs buffers'),
        h('div', { class: 'from' }, 'pai’s other screens and buffers opened from here'),
        h('div', { class: 'list' }, ...(S.buffers.length ? S.buffers.map(b => h('button', {
          onclick: () => { UI.closeSheet(true); Buffer.open(b.b); } }, `${b.name}  ·  ${b.mode}`))
          : [h('div', { class: 'from' }, 'None open')])),
        h('div', { class: 'btns' }, h('button', { onclick: () => UI.closeSheet() }, 'Close')));
    });
  }

  function settings() {
    const p = UI.prefs;
    const check = (label, key, onChange) => {
      const box = h('input', { type: 'checkbox' });
      box.checked = !!p[key];
      box.addEventListener('change', async () => {
        p[key] = box.checked;
        if (onChange) p[key] = await onChange(box.checked);
        box.checked = !!p[key];
        UI.savePrefs();
      });
      return h('label', { class: 'row' }, box, label);
    };
    const theme = h('select', {}, ...['system', 'dark', 'light'].map(t => {
      const o = h('option', { value: t }, `Theme: ${t}`); if (p.theme === t) o.selected = true; return o;
    }));
    theme.addEventListener('change', () => { p.theme = theme.value; UI.savePrefs(); });
    UI.openSheet('settings', sheet => {
      sheet.append(h('h3', {}, 'This browser'),
        check('Sound when an instance needs you', 'sound', v => { if (v) UI.beep(); return v; }),
        check('System notifications', 'notify', v => v ? UI.enableNotifications() : false),
        theme,
        h('div', { class: 'from' }, 'Server settings (port, password, attachments) are in Emacs: /menu → Web.'),
        h('div', { class: 'btns' },
          h('button', { class: 'danger', onclick: async () => {
            try { await Net.post('/api/logout', {}); } catch (_) { /* gone */ }
            location.reload();
          } }, 'Log out'),
          h('button', { onclick: () => UI.closeSheet() }, 'Close')));
    });
  }

  $('#new-btn').addEventListener('click', newInstance);
  $('#menu-btn').addEventListener('click', settings);
  $('#buffers-btn').addEventListener('click', buffers);

  return { prompt, ask, model, thinking, newInstance, chatMenu, buffers, settings, confirm };
})();
