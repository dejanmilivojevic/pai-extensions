---
name: visualize
description: "Add a correct, minimal visual to a lesson — a diagram or geometric picture — that shows inline in the Org lesson log. Use when an idea is genuinely clearer as a picture: a dependency graph, system/flow, sequence, state machine, tree, comparison, or a spatial/geometric thing (coordinate geometry, number line, vectors, a plot, a physical layout). Outsources authoring+rendering to a maker subagent that verifies the image by looking at it, then you embed the returned file."
---

# Visualize

A picture earns its place only when it shows something words can't — shape, structure, direction, relationship, geometry. This skill produces ONE such picture, guarantees it is **correct** (the maker renders it and looks at it before returning), and drops it into the lesson so it shows inline in the Org lesson log (`/teach-log`).

You are the **creative director**. You decide the exact idea and distill it to its fewest carrying elements. A **maker subagent** does the authoring, rendering, visual verification, and saving, then returns a file path. You embed that path in your reply.

## When to visualize (and when not to)

This teaching system builds a **dependency graph in the learner's head** — axioms at the root, derived facts hanging off them. A visual is powerful exactly when it makes that structure (or a geometry) visible. Reach for one when:

- The idea is a **structure or relationship**: dependencies, a system with parts and arrows, a flow/pipeline, a sequence of exchanges, a state machine, a tree/hierarchy, a comparison, a containment (what's inside vs outside).
- The idea is **spatial or geometric**: coordinate geometry, a number line, vectors, a function's shape, a physical arrangement.

Do NOT visualize when prose or a single equation already carries it. A decorative diagram that just restates the sentence next to it adds noise and a chance to be wrong. When in doubt, don't — a missing visual is cheaper than a false one.

(The Phase 2 dependency map is the exception: write it as a plain ```mermaid``` block in your reply. The lesson log renders mermaid blocks to images by itself — no maker needed.)

## Choose the maker

Two maker roles are available to the `subagent` tool in a teaching session:

- **`mermaid-maker`** — structural/relational visuals: dependency graphs, flowcharts, sequence/state/ER/class diagrams, trees, mindmaps, timelines. This is the default and fits the dependency-graph pedagogy directly.
- **`svg-maker`** — spatial/geometric visuals Mermaid can't lay out: exact coordinates, geometry figures, number lines, vectors, plots, custom shapes.

Rule of thumb: if it's *nodes-and-edges / relationships*, use mermaid-maker. If it's *positions-and-shapes / geometry*, use svg-maker.

## Brief the maker well: one idea, fewest elements

The most common failure is **cramming** — every extra label makes the picture harder to read AND harder to lay out correctly. Before briefing, prune to the fewest elements that carry the idea, and for each ask: *"if I delete this, is the idea still clear?"* If yes, delete it.

Give the maker the concept AND the concrete elements you want — not a vague topic, and not a long checklist. The maker starts fresh: it knows nothing of the lesson except what you put in the brief.

- BAD: "make a diagram about how TCP works"
- GOOD: "graph TD: a node 'packet' at the top; arrows down to 'ordering' and 'retransmit on loss'; both arrows down into 'reliable stream'. No title. Show that reliability is built FROM packets, not alongside them."

Keep the idea intact but trust the maker to compose; if your brief lists more than ~5–7 elements, cut it first.

## Invoke

Dispatch the maker with the `subagent` tool and wait for its answer:

```
subagent(agent="mermaid-maker", task="<your minimal, concrete brief>", async=false)
```
```
subagent(agent="svg-maker", task="<your minimal, concrete brief>", async=false)
```

The maker owns the `render_diagram` tool — it authors the source, renders it to a PNG (inside Emacs), **looks at the PNG and iterates until it is correct and clean**, publishes it with a unique filename (next to the lesson log, in `viz/`, or in `viz/` under the working directory when no log is linked), and ends its reply with:

```
RESULT:
filename: viz-<slug>-<timestamp>.png
path: /absolute/path/viz-<slug>-<timestamp>.png
```

If it returns `RESULT: NONE`, it couldn't make a correct picture of the brief — simplify or rethink, or decide the visual isn't worth it. Never hand-author or fake a diagram yourself; correctness depends on the maker's render-and-inspect loop.

Once you have the result, close the maker: `subagent(action="close", id=...)`. For a revision, `subagent(action="say", id=..., task="<what to change>", async=false)` before closing instead of starting over.

## Embed it in the lesson

Put a Markdown image with the returned **absolute path** on its own line in your teaching reply:

```
![short caption](/absolute/path/viz-<slug>-<timestamp>.png)
```

The lesson log converts it to an inline Org image at a readable width. Introduce the visual in a sentence, then let it carry the idea — don't narrate every element back in prose.

## Why this is reliable

- The maker never returns a picture it hasn't **looked at**, so "renders fine but says something false" is caught before it reaches the learner.
- PNG embed means **what the maker verified is pixel-identical to what the learner sees** — no re-render drift.
- Unique filenames keep embeds unambiguous.

> The makers render with pai-learn's in-Emacs renderer: Mermaid through mermaid.js and SVG directly, both rasterised by the xwidget-webkit engine built into Emacs. You don't render anything yourself — you only brief the maker and embed the path it returns.
