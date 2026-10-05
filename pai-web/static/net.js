// pai-web network layer: JSON API calls (big answers arrive as "blobs",
// downloaded piece by piece), and the long-poll loop that receives events.
'use strict';

const Net = (() => {
  const dec = new TextDecoder();

  class HttpError extends Error {
    constructor(status, message) { super(message); this.status = status; }
  }

  async function blob(id, size) {
    const parts = [];
    let got = 0;
    while (got < size) {
      const r = await fetch(`/api/blob?id=${encodeURIComponent(id)}&o=${got}`, { credentials: 'same-origin' });
      if (!r.ok) throw new HttpError(r.status, 'Download failed');
      const buf = new Uint8Array(await r.arrayBuffer());
      if (!buf.length) break;
      parts.push(buf);
      got += buf.length;
    }
    const all = new Uint8Array(got);
    let o = 0;
    for (const p of parts) { all.set(p, o); o += p.length; }
    return JSON.parse(dec.decode(all));
  }

  async function unwrap(r) {
    let data = null;
    const text = await r.text();
    try { data = text ? JSON.parse(text) : null; } catch (_) { data = { error: text }; }
    if (!r.ok) throw new HttpError(r.status, (data && data.error) || `HTTP ${r.status}`);
    if (data && typeof data.$blob === 'string') return blob(data.$blob, data.size);
    return data;
  }

  async function get(path, params) {
    const q = params ? '?' + new URLSearchParams(params).toString() : '';
    const r = await fetch(path + q, { credentials: 'same-origin' });
    return unwrap(r);
  }

  async function post(path, body, raw) {
    const r = await fetch(path, {
      method: 'POST', credentials: 'same-origin',
      headers: { 'X-Pai': '1', 'Content-Type': raw ? 'application/octet-stream' : 'application/json' },
      body: raw ? body : JSON.stringify(body || {}),
    });
    return unwrap(r);
  }

  // Long polling.  onEvents(events) may return a promise; the next poll
  // acknowledges what was processed.  onStatus(state) gets "on", "off",
  // "login" or "gone" (the server forgot this page: reconnect).
  function poller(client, onEvents, onStatus) {
    let ack = 0, stopped = false, failures = 0, ctrl = null, kicked = false;
    async function loop() {
      while (!stopped) {
        try {
          ctrl = new AbortController();
          const timer = setTimeout(() => ctrl.abort(), 40000);
          const r = await fetch(`/api/poll?c=${client}&ack=${ack}`,
                                { credentials: 'same-origin', signal: ctrl.signal });
          clearTimeout(timer);
          if (r.status === 401) { onStatus('login'); return; }
          if (r.status === 410) { onStatus('gone'); return; }
          if (!r.ok) throw new Error(`HTTP ${r.status}`);
          const data = await r.json();
          failures = 0;
          onStatus('on');
          const events = [];
          for (const e of data.events || []) {
            events.push(e.t === 'blob' ? await blob(e.blob, e.size) : e);
          }
          if (events.length) {
            await onEvents(events);
            ack = data.seq;
          }
        } catch (err) {
          if (stopped) return;
          if (kicked) { kicked = false; continue; }
          failures++;
          onStatus('off');
          await new Promise(res => setTimeout(res, Math.min(10000, 500 * 2 ** Math.min(failures, 5))));
        }
      }
    }
    loop();
    return {
      stop() { stopped = true; if (ctrl) ctrl.abort(); },
      kick() { kicked = true; if (ctrl) ctrl.abort(); },   // retry now (page became visible)
    };
  }

  return { get, post, blob, poller, HttpError };
})();
