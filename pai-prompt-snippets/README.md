# Prompt Snippets

An Emacs port of the [`prompt-snippets`][upstream] pi extension.

Mix-and-match single-purpose prompt rules that are prepended or appended to
your message when you send it. Unlike skills, each snippet is a tiny,
standalone instruction — toggle exactly the ones you want per message.

## Usage

- Press **`C-c s`** or run **`/snippets`** to open the toggle menu (a `vui`
  buffer):
  - `TAB`/`S-TAB` navigate, `SPC`/`RET` toggle, `C-c C-c` apply, `C-c C-k`/`q`
    cancel.
  - `p` previews the snippet at point (name, placement, order, filename and
    full body) in its own read-only buffer.
  - Snippets are grouped into a `↑ PREPEND` and a `↓ APPEND` section.
- Active snippets show up as a mode-line widget:
  - `↑ prepend: …` (accent) — inserted before your message
  - `↓ append: …` (warning) — inserted after your message
- When you send a message, active snippet bodies are merged into the message
  text: prepend group (sorted by `order`) → your text → append group (sorted
  by `order`), separated by blank lines.
- Toggles reset to **all off** after each send and at session start.

## Snippet files

Snippets live in `snippets/` next to the extension — one markdown file each,
with frontmatter:

```markdown
---
name: Concise
description: Keep answers short and to the point
placement: prepend
order: 10
---
Keep your response concise. Skip preamble and unnecessary explanation.
```

| Field | Required | Notes |
|---|---|---|
| `name` | no | Display name; defaults to the filename without `.md` |
| `description` | no | Shown next to the name in the toggle menu |
| `placement` | no | `prepend` or `append` (default: `append`) |
| `order` | no | Sorts snippets within their group, in the menu and the applied text (default: `9999`, ties broken by name) |

Files are re-scanned every time the menu opens and every time a message is
sent, so edits take effect immediately — no `/reload` needed.

### Where snippets are read from

In precedence order (later directories win when two files share a name):

1. The bundled `snippets/` directory next to `pai-prompt-snippets.el`.
2. Your own `~/.pai/snippets/` (`pai-prompt-snippets-user-dir`).
3. Any directory in the `pai-prompt-snippets-directories` variable (editable
   under *Prompt snippets → Sources* in `/menu`).
4. `.pai/snippets/` under the project directory.

## Learned snippets

With pai-memory, snippets are learned like skills. When you attach the same
kind of short instruction to your messages ("keep it short", "show the plan
first"), the observer records an `instruction:` and the promoter may propose a
`snippet-create` (or `snippet-patch`) for `/memory-review`. Accepted snippets
are written to `~/.pai/snippets/NAME.md` (or the trusted project's
`.pai/snippets/`) with `origin: learned` in their frontmatter, and can be
undone with `/memory undo`. `pai-prompt-snippets-list` is the public listing
the promoter uses.

So a project or user override shadows a bundled snippet of the same filename.

## Snippets and memory

Each merged snippet is wrapped in a `<prompt-snippet name="…">` block. The
model reads it as before, but pai-memory drops these blocks before observing,
recalling or indexing a message, so a snippet you use often is never learned
as something *you* keep asking for. (Loaded skills are treated alike: memory
keeps only a `[skill: NAME]` marker, not the skill's body.)

## Differences from upstream

The snippet format is identical to the [pi extension][upstream]; merged
snippets are additionally tagged (see above).
The user interface is native Emacs rather than a pi-tui overlay: the toggle
menu is a real `vui` buffer, the preview is an ordinary read-only buffer, and
the active-snippets indicator is a mode-line widget. Upstream binds the menu to
`alt+s`; here it is `C-c s`, because `M-s` is the standard Emacs search prefix.

[upstream]: https://github.com/amosblomqvist/pi-config/tree/main/extensions/prompt-snippets
