;;; pai-browser-js.el --- JavaScript builders for pai-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure functions returning JavaScript source.  Page-side snippets are
;; zero-argument function declarations accepted by both Playwright's
;; `browser_evaluate' and chrome-devtools' `evaluate_script'; they always
;; return a JSON *string* ({ok, ...}) so results survive both servers'
;; text formatting unchanged.  Everything is injected through CDP
;; evaluation, which page CSP does not govern.

;;; Code:

(require 'subr-x)
(require 'pai-core)

(defun pai-browser-js-str (value)
  "Return VALUE (a string or nil) as a JavaScript string literal or null."
  (if (null value) "null"
    (let ((s (pai-json-encode (vector value))))
      (substring s 1 -1))))

(defun pai-browser-js-json (value)
  "Return VALUE encoded as a JSON/JavaScript literal (objects, arrays, scalars)."
  (let ((s (pai-json-encode (vector value))))
    (substring s 1 -1)))

(defun pai-browser-js--guarded (body timeout-ms)
  "Wrap async JS BODY (statements returning an object) with TIMEOUT-MS.
Errors and timeouts become {ok: false, error}."
  (format "async () => {
  const __ms = %d;
  const __run = async () => { %s };
  try {
    const v = await Promise.race([__run(), new Promise((_, rej) =>
      setTimeout(() => rej(new Error('timed out after ' + __ms + 'ms')), __ms))]);
    return JSON.stringify(Object.assign({ok: true}, v));
  } catch (e) {
    return JSON.stringify({ok: false, error: String((e && e.stack) || e)});
  }
}" timeout-ms body))

(defun pai-browser-js-exec (code &optional timeout-ms)
  "JS running user CODE (a function source or expression) in the page."
  (pai-browser-js--guarded
   (format "const __f = (%s);
    const __v = (typeof __f === 'function') ? await __f() : __f;
    return {value: __v === undefined ? null : __v};" code)
   (or timeout-ms 30000)))

(defun pai-browser-js-page-info ()
  "JS returning url, title, readyState and the current selection."
  (pai-browser-js--guarded
   "return {url: location.href, title: document.title, readyState: document.readyState,
      selection: String(window.getSelection ? window.getSelection() : '')};"
   10000))

(defun pai-browser-js-fetch (url method headers as-base64 &optional timeout-ms)
  "JS doing a same-origin read-only fetch of URL with METHOD and HEADERS plist.
AS-BASE64 non-nil returns the body base64-encoded."
  (pai-browser-js--guarded
   (format "const u = new URL(%s, location.href);
    if (u.origin !== location.origin)
      throw new Error('cross-origin fetch refused: ' + u.origin + ' is not the page origin ' + location.origin);
    const method = %s;
    const r = await fetch(u.href, {method, headers: %s, credentials: 'include', redirect: 'follow'});
    const headers = {}; r.headers.forEach((v, k) => { headers[k] = v; });
    let body = '', encoding = 'text';
    if (method !== 'HEAD') {
      if (%s) {
        const b = new Uint8Array(await r.arrayBuffer()); let s = '';
        for (let i = 0; i < b.length; i += 0x8000) s += String.fromCharCode.apply(null, b.subarray(i, i + 0x8000));
        body = btoa(s); encoding = 'base64';
      } else { body = await r.text(); }
    }
    return {status: r.status, statusText: r.statusText, url: r.url, redirected: r.redirected,
            contentType: r.headers.get('content-type'), headers, encoding, body};"
           (pai-browser-js-str url) (pai-browser-js-str method)
           (if headers (pai-json-encode headers) "{}")
           (if as-base64 "true" "false"))
   (or timeout-ms 60000)))

(defun pai-browser-js-element-center (selector)
  "JS scrolling SELECTOR into view and returning its viewport center."
  (pai-browser-js--guarded
   (format "const el = document.querySelector(%s);
    if (!el) throw new Error('no element matches ' + %s);
    el.scrollIntoView({block: 'center', inline: 'center'});
    const r = el.getBoundingClientRect();
    return {x: r.left + r.width / 2, y: r.top + r.height / 2};"
           (pai-browser-js-str selector) (pai-browser-js-str selector))
   10000))

(defun pai-browser-js-select (selector value)
  "JS setting the <select> SELECTOR to VALUE and dispatching input/change."
  (pai-browser-js--guarded
   (format "const el = document.querySelector(%s);
    if (!el) throw new Error('no element matches ' + %s);
    el.value = %s;
    el.dispatchEvent(new Event('input', {bubbles: true}));
    el.dispatchEvent(new Event('change', {bubbles: true}));
    return {value: el.value};"
           (pai-browser-js-str selector) (pai-browser-js-str selector) (pai-browser-js-str value))
   10000))

(defun pai-browser-js-scroll (selector dx dy)
  "JS scrolling SELECTOR into view, or the window by DX/DY."
  (pai-browser-js--guarded
   (if selector
       (format "const el = document.querySelector(%s);
    if (!el) throw new Error('no element matches ' + %s);
    el.scrollIntoView({block: 'center'}); return {scrolled: 'element'};"
               (pai-browser-js-str selector) (pai-browser-js-str selector))
     (format "window.scrollBy(%s, %s); return {scrollX: window.scrollX, scrollY: window.scrollY};"
             (or dx 0) (or dy 0)))
   10000))

(defun pai-browser-js-sleep (ms)
  "JS resolving after MS milliseconds."
  (pai-browser-js--guarded
   (format "await new Promise(r => setTimeout(r, %d)); return {waited: %d};" ms ms)
   (+ ms 5000)))

;;;; Playwright page-level code (browser_run_code_unsafe)

(defun pai-browser-js-pw (body)
  "Wrap BODY (statements using `page', returning an object) for run_code."
  (format "async (page) => {
  try { const v = await (async () => { %s })(); return JSON.stringify(Object.assign({ok: true}, v || {})); }
  catch (e) { return JSON.stringify({ok: false, error: String((e && e.message) || e)}); }
}" body))

(defun pai-browser-js-pw-point (selector x y)
  "Playwright statements binding `x', `y' to SELECTOR's center or X/Y."
  (if selector
      (format "const __b = await page.locator(%s).first().boundingBox();
      if (!__b) throw new Error('no visible element matches ' + %s);
      const x = __b.x + __b.width / 2, y = __b.y + __b.height / 2;"
              (pai-browser-js-str selector) (pai-browser-js-str selector))
    (format "const x = %s, y = %s;" (or x 0) (or y 0))))

(defun pai-browser-js-pw-tap (selector x y)
  "Playwright code tapping SELECTOR or X/Y with CDP touch events."
  (pai-browser-js-pw
   (concat (pai-browser-js-pw-point selector x y)
           "const s = await page.context().newCDPSession(page);
      await s.send('Emulation.setTouchEmulationEnabled', {enabled: true, maxTouchPoints: 1});
      await s.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [{x, y}]});
      await s.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
      await s.detach();
      return {tapped: [x, y]};")))

(defun pai-browser-js-pw-bypass-csp ()
  "Playwright code enabling CSP bypass for the page (effective on next load)."
  (pai-browser-js-pw
   "const s = await page.context().newCDPSession(page);
    await s.send('Page.setBypassCSP', {enabled: true}); await s.detach();
    return {bypassCSP: true};"))

;;;; Annotation overlay

(defconst pai-browser-js--annotate-start
  "() => {
  if (window.__paiAnnotate && window.__paiAnnotate.active) return JSON.stringify({ok: true, already: true, count: (window.__paiAnnotations || []).length});
  window.__paiAnnotations = window.__paiAnnotations || [];
  const st = window.__paiAnnotate = {active: true, nodes: []};
  const css = (el, props) => { for (const k in props) el.style.setProperty(k, props[k], 'important'); return el; };
  const box = css(document.createElement('div'), {position: 'fixed', 'pointer-events': 'none', border: '2px solid #e0457b',
    background: 'rgba(224,69,123,0.08)', 'z-index': '2147483646', display: 'none'});
  const bar = css(document.createElement('div'), {position: 'fixed', top: '8px', right: '8px', 'z-index': '2147483647',
    background: '#222', color: '#fff', font: '12px sans-serif', padding: '6px 10px', 'border-radius': '6px'});
  bar.textContent = 'pai annotate: click elements to mark, Esc to stop';
  document.documentElement.append(box, bar); st.nodes.push(box, bar);
  const sel = (el) => {
    if (el.id) return '#' + CSS.escape(el.id);
    const parts = [];
    for (let e = el; e && e.nodeType === 1 && e !== document.documentElement; e = e.parentElement) {
      if (e.id) { parts.unshift('#' + CSS.escape(e.id)); break; }
      let i = 1; for (let s = e.previousElementSibling; s; s = s.previousElementSibling) if (s.tagName === e.tagName) i++;
      parts.unshift(e.tagName.toLowerCase() + ':nth-of-type(' + i + ')');
    }
    return parts.join(' > ');
  };
  const ours = (el) => st.nodes.some(n => n === el || n.contains(el));
  const move = (ev) => { const el = ev.target; if (ours(el)) return; const r = el.getBoundingClientRect();
    css(box, {display: 'block', left: r.left + 'px', top: r.top + 'px', width: r.width + 'px', height: r.height + 'px'}); };
  const click = (ev) => {
    const el = ev.target; if (ours(el)) return;
    ev.preventDefault(); ev.stopPropagation(); ev.stopImmediatePropagation();
    const r = el.getBoundingClientRect(); const n = window.__paiAnnotations.length + 1;
    const a = {n, selector: sel(el), text: (el.innerText || el.value || '').trim().slice(0, 300), note: '',
      rect: {x: Math.round(r.left + scrollX), y: Math.round(r.top + scrollY), width: Math.round(r.width), height: Math.round(r.height)},
      html: el.outerHTML.slice(0, 500)};
    window.__paiAnnotations.push(a);
    const badge = css(document.createElement('div'), {position: 'absolute', left: (r.left + scrollX) + 'px', top: (r.top + scrollY - 22) + 'px',
      'z-index': '2147483647', background: '#e0457b', color: '#fff', font: 'bold 12px sans-serif', padding: '2px 6px', 'border-radius': '10px'});
    badge.textContent = String(n);
    const input = css(document.createElement('input'), {position: 'absolute', left: (r.left + scrollX + 26) + 'px', top: (r.top + scrollY - 24) + 'px',
      'z-index': '2147483647', font: '12px sans-serif', width: '260px', padding: '2px 4px', border: '1px solid #e0457b', background: '#fff', color: '#000'});
    input.placeholder = 'note for #' + n + ' (Enter to save)';
    input.addEventListener('keydown', (e) => { e.stopPropagation(); if (e.key === 'Enter' || e.key === 'Escape') { a.note = input.value; input.remove(); } });
    input.addEventListener('input', () => { a.note = input.value; });
    document.body.append(badge, input); st.nodes.push(badge, input); input.focus();
  };
  const key = (ev) => { if (ev.key === 'Escape' && !ours(ev.target)) stop(); };
  const stop = () => { st.active = false; box.remove(); bar.remove();
    document.removeEventListener('mousemove', move, true); document.removeEventListener('click', click, true);
    document.removeEventListener('keydown', key, true); };
  st.stop = stop;
  document.addEventListener('mousemove', move, true); document.addEventListener('click', click, true);
  document.addEventListener('keydown', key, true);
  return JSON.stringify({ok: true, started: true, count: window.__paiAnnotations.length});
}"
  "JS installing the annotation overlay (CSP-safe: CSSOM styles, listeners only).")

(defun pai-browser-js-annotate-start ()
  "JS starting annotation mode in the current page."
  pai-browser-js--annotate-start)

(defun pai-browser-js-annotations (finish clear)
  "JS returning the page annotations.
FINISH removes the overlay, CLEAR resets the list."
  (format "() => {
  const list = (window.__paiAnnotations || []).slice();
  const st = window.__paiAnnotate;
  if (%s && st) { if (st.stop) st.stop(); (st.nodes || []).forEach(n => n.remove()); window.__paiAnnotate = null; }
  if (%s) window.__paiAnnotations = [];
  return JSON.stringify({ok: true, url: location.href, title: document.title, active: !!(st && st.active), annotations: list});
}" (if finish "true" "false") (if clear "true" "false")))

(provide 'pai-browser-js)
;;; pai-browser-js.el ends here
