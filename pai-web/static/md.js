// A small Markdown renderer.  All text is escaped first; only the tags
// below are produced.  Code fences are found the way pai-web-instances.el
// finds them, so FENCES[i] (HTML highlighted by Emacs, or null) belongs to
// the i-th fence.
'use strict';

const MD = (() => {
  const esc = s => s.replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

  function safeUrl(u) {
    u = u.trim();
    return /^(https?:|mailto:)/i.test(u) ? u : null;
  }

  // inline markup on escaped text
  function inline(raw) {
    const codes = [];
    let s = raw.replace(/`([^`\n]+)`/g, (_, c) => { codes.push(c); return `\u0000${codes.length - 1}\u0000`; });
    s = esc(s);
    s = s.replace(/\[([^\]\n]+)\]\(([^)\s]+)\)/g, (m, t, u) => {
      const url = safeUrl(u.replace(/&amp;/g, '&'));
      return url ? `<a href="${esc(url)}" target="_blank" rel="noopener noreferrer">${t}</a>` : m;
    });
    s = s.replace(/(^|[\s(])(https?:\/\/[^\s<)]+[^\s<).,;:!?'"])/g,
                  (m, pre, u) => `${pre}<a href="${u}" target="_blank" rel="noopener noreferrer">${u}</a>`);
    s = s.replace(/\*\*([^*\n]+)\*\*/g, '<strong>$1</strong>')
         .replace(/__([^_\n]+)__/g, '<strong>$1</strong>')
         .replace(/(^|[^*\w])\*([^*\n]+)\*(?!\w)/g, '$1<em>$2</em>')
         .replace(/(^|[^_\w])_([^_\n]+)_(?!\w)/g, '$1<em>$2</em>')
         .replace(/~~([^~\n]+)~~/g, '<del>$1</del>');
    return s.replace(/\u0000(\d+)\u0000/g, (_, i) => `<code>${esc(codes[+i])}</code>`);
  }

  function table(lines) {
    const cells = l => l.trim().replace(/^\|/, '').replace(/\|$/, '').split('|').map(c => c.trim());
    const head = cells(lines[0]);
    const rows = lines.slice(2).map(cells);
    return '<table><thead><tr>' + head.map(h => `<th>${inline(h)}</th>`).join('') + '</tr></thead><tbody>' +
      rows.map(r => '<tr>' + r.map(c => `<td>${inline(c)}</td>`).join('') + '</tr>').join('') + '</tbody></table>';
  }

  function list(lines) {
    // nested lists by indentation
    const out = [];
    const stack = [];
    for (const l of lines) {
      const m = l.match(/^(\s*)([-*+]|\d+[.)])\s+(.*)$/);
      if (!m) { if (out.length) out[out.length - 1] = out[out.length - 1].replace(/<\/li>$/, ' ' + inline(l.trim()) + '</li>'); continue; }
      const indent = m[1].replace(/\t/g, '    ').length;
      const tag = /\d/.test(m[2]) ? 'ol' : 'ul';
      while (stack.length && indent < stack[stack.length - 1].indent) out.push(`</${stack.pop().tag}>`);
      if (stack.length && indent === stack[stack.length - 1].indent && tag !== stack[stack.length - 1].tag) {
        out.push(`</${stack.pop().tag}>`);
      }
      if (!stack.length || indent > stack[stack.length - 1].indent) {
        stack.push({ indent, tag }); out.push(`<${tag}>`);
      }
      let item = m[3];
      const box = item.match(/^\[([ xX])\]\s+(.*)$/);
      item = box ? (box[1] === ' ' ? '☐ ' : '☑ ') + inline(box[2]) : inline(item);
      out.push(`<li>${item}</li>`);
    }
    while (stack.length) out.push(`</${stack.pop().tag}>`);
    return out.join('');
  }

  function render(text, fences) {
    const lines = (text || '').split('\n');
    const out = [];
    let i = 0, fence = 0, para = [];
    const flush = () => { if (para.length) { out.push('<p>' + inline(para.join('\n')).replace(/\n/g, '<br>') + '</p>'); para = []; } };
    while (i < lines.length) {
      const l = lines[i];
      const f = l.match(/^[ \t]*(`{3,}|~{3,})[ \t]*([^`\n]*)$/);
      if (f && i + 1 <= lines.length) {
        flush();
        const marker = f[1], lang = f[2].trim();
        const close = new RegExp('^[ \\t]*' + marker.replace(/[~`]/g, c => '\\' + c) + '[ \\t]*$');
        const body = [];
        i++;
        while (i < lines.length && !close.test(lines[i])) body.push(lines[i++]);
        i++;
        const hl = fences && fences[fence];
        fence++;
        if (lang) out.push(`<div class="lang">${esc(lang)}</div>`);
        out.push(`<pre class="code"><code>${hl != null ? hl : esc(body.join('\n'))}</code></pre>`);
        continue;
      }
      if (/^\s*$/.test(l)) { flush(); i++; continue; }
      const h = l.match(/^(#{1,6})\s+(.*)$/);
      if (h) { flush(); out.push(`<h${h[1].length}>${inline(h[2])}</h${h[1].length}>`); i++; continue; }
      if (/^\s*([-*_])(\s*\1){2,}\s*$/.test(l)) { flush(); out.push('<hr>'); i++; continue; }
      if (/^\s*>/.test(l)) {
        flush();
        const q = [];
        while (i < lines.length && /^\s*>/.test(lines[i])) q.push(lines[i++].replace(/^\s*>\s?/, ''));
        out.push('<blockquote>' + render(q.join('\n'), null) + '</blockquote>');
        continue;
      }
      if (/^\s*\|.*\|\s*$/.test(l) && i + 1 < lines.length && /^\s*\|?\s*:?-{2,}/.test(lines[i + 1])) {
        flush();
        const t = [];
        while (i < lines.length && /^\s*\|.*\|\s*$/.test(lines[i])) t.push(lines[i++]);
        out.push(table(t));
        continue;
      }
      if (/^\s*([-*+]|\d+[.)])\s+/.test(l)) {
        flush();
        const items = [];
        while (i < lines.length && (/^\s*([-*+]|\d+[.)])\s+/.test(lines[i]) ||
                                    (/^\s{2,}\S/.test(lines[i]) && items.length))) items.push(lines[i++]);
        out.push(list(items));
        continue;
      }
      para.push(l);
      i++;
    }
    flush();
    return out.join('');
  }

  // number of code fences in TEXT, found exactly as `render' finds them
  function countFences(text) {
    const lines = (text || '').split('\n');
    let n = 0;
    for (let i = 0; i < lines.length; i++) {
      const f = lines[i].match(/^[ \t]*(`{3,}|~{3,})[ \t]*([^`\n]*)$/);
      if (!f) continue;
      n++;
      const close = new RegExp('^[ \\t]*' + f[1].replace(/[~`]/g, c => '\\' + c) + '[ \\t]*$');
      i++;
      while (i < lines.length && !close.test(lines[i])) i++;
    }
    return n;
  }

  return { render, esc, inline, countFences };
})();
