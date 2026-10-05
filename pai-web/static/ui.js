// DOM helpers, toasts, the bottom sheet, preferences, sound, notifications.
'use strict';

const $ = sel => document.querySelector(sel);

function h(tag, attrs, ...kids) {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs || {})) {
    if (v == null || v === false) continue;
    if (k === 'class') e.className = v;
    else if (k === 'html') e.innerHTML = v;
    else if (k.startsWith('on')) e.addEventListener(k.slice(2), v);
    else if (k === 'dataset') Object.assign(e.dataset, v);
    else e.setAttribute(k, v === true ? '' : v);
  }
  for (const k of kids.flat()) {
    if (k == null || k === false) continue;
    e.append(k instanceof Node ? k : document.createTextNode(String(k)));
  }
  return e;
}

const UI = (() => {
  const prefs = Object.assign({ sound: true, notify: false, theme: 'system', aboveCollapsed: false },
                              JSON.parse(localStorage.getItem('pai-web-prefs') || '{}'));

  function savePrefs() { localStorage.setItem('pai-web-prefs', JSON.stringify(prefs)); applyTheme(); }

  function applyTheme() {
    if (prefs.theme === 'system') delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = prefs.theme;
  }
  applyTheme();

  function toast(text, level) {
    const t = h('div', { class: `toast ${level || 'info'}` }, text);
    $('#toasts').append(t);
    t.addEventListener('click', () => t.remove());
    setTimeout(() => t.remove(), level === 'error' ? 7000 : 3500);
  }

  // One sheet at a time; the newest replaces the previous one.  onClose
  // runs when it is dismissed (backdrop, Escape, or closeSheet()).
  let sheetClose = null, sheetKey = null;
  function openSheet(key, build, onClose) {
    closeSheet(true);
    const sheet = $('#sheet');
    sheet.replaceChildren();
    build(sheet);
    sheetKey = key;
    sheetClose = onClose || null;
    sheet.classList.remove('hidden');
    $('#sheet-backdrop').classList.remove('hidden');
    const first = sheet.querySelector('input:not([type=checkbox]), textarea');
    if (first && !matchMedia('(pointer: coarse)').matches) setTimeout(() => first.focus(), 30);
  }
  function closeSheet(silent) {
    const fn = sheetClose;
    sheetClose = null; sheetKey = null;
    $('#sheet').classList.add('hidden');
    $('#sheet-backdrop').classList.add('hidden');
    if (fn && !silent) fn();
  }
  function currentSheet() { return sheetKey; }
  $('#sheet-backdrop').addEventListener('click', () => closeSheet());
  document.addEventListener('keydown', e => { if (e.key === 'Escape' && sheetKey) closeSheet(); });

  let audio = null;
  function beep() {
    if (!prefs.sound) return;
    try {
      audio = audio || new (window.AudioContext || window.webkitAudioContext)();
      const o = audio.createOscillator(), g = audio.createGain();
      o.frequency.value = 880; g.gain.value = 0.06;
      o.connect(g); g.connect(audio.destination);
      o.start(); o.frequency.setValueAtTime(660, audio.currentTime + 0.12);
      o.stop(audio.currentTime + 0.25);
    } catch (_) { /* no audio */ }
  }

  function notify(title, body, onClick) {
    beep();
    if (!prefs.notify || !('Notification' in window) || Notification.permission !== 'granted') return;
    if (!document.hidden) return;
    try {
      const n = new Notification(title, { body, tag: title });
      n.onclick = () => { window.focus(); n.close(); if (onClick) onClick(); };
    } catch (_) { /* not allowed here (e.g. plain http on a phone) */ }
  }

  async function enableNotifications() {
    if (!('Notification' in window)) { toast('This browser has no notifications', 'warning'); return false; }
    if (!window.isSecureContext) {
      toast('System notifications need https or localhost; badges and sound still work', 'warning');
      return false;
    }
    const p = await Notification.requestPermission();
    return p === 'granted';
  }

  const fmtTokens = n => n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(1) + 'k' : String(n);

  return { prefs, savePrefs, toast, openSheet, closeSheet, currentSheet, notify, beep,
           enableNotifications, fmtTokens };
})();
