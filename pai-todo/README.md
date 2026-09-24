# pai-todo

A phased todo list the agent keeps while it works: a port of oh-my-pi's
`todo` tool (`packages/coding-agent/src/tools/todo.ts`,
`session/todo-tracker.ts`, `slash-commands/helpers/todo.ts`).

## Tool `todo`

One operation per call:

| op | arguments | effect |
|---|---|---|
| `init` | `list: [{phase, items}]`, or `items` | replace the whole list |
| `start` | `task` | mark in progress |
| `done` / `drop` | `task` or `phase` | completed / abandoned |
| `block` | `task` or `phase`, `reason?` | waiting on something external |
| `unblock` | `task` or `phase` | blocked -> pending |
| `rm` | `task`, `phase`, or nothing (clear) | remove |
| `append` | `phase`, `items` | add tasks (creates the phase) |
| `view` | | read-only |

- Tasks and phases are addressed by exact text, never ids.
- After every change exactly one task is in progress (the earliest pending one
  is promoted; blocked tasks never are).
- A call with any error changes nothing; a missing `op` is inferred only when
  unambiguous (`list` -> init, `items`+`phase` -> append, bare `items` with no
  list yet -> init).
- The schema is always sent (`:deferred nil`): no reveal round trip.

## State

Per session, from its current branch: the latest successful `todo` tool result
(its `:details (:op :phases)`), or the latest `/todo` edit (a `custom` entry of
type `"todo"`). Resume, `/tree` and forks therefore show the list of that
point in the conversation. Differences from upstream: no sticky HUD (a status
bar widget instead), no eager "create a todo first" prelude and no mid-run
nudge.

## Reminders

When a run settles with pending or in-progress tasks, the agent is sent
`[todo reminder N/MAX] You stopped with ... open todo items` (as a visible
prompt). Skipped after an abort/error, when the last answer ends with a
question, while the previous reminder got no tool call, and after
`:reminders-max` (default 2) reminders per user prompt. Settings: `:todo
(:reminders BOOL :reminders-max N)`, also under *Todo* in `/menu`.

## /todo

`show | start|done|drop|block|unblock|rm TASK-or-PHASE | append [PHASE:] TASK |
clear | edit | export [FILE] | import [FILE] | reminders on|off`

Matching is case-insensitive: exact, else a unique substring (open tasks
preferred). Task names complete as one argument even with spaces (the core
`(:line ...)` completion node). `edit`, `export` and `import` use an Org
outline:

```org
#+TODO: TODO DOING BLOCKED | DONE DROPPED
* Foundation
** DONE Scaffold crate
** DOING Wire workspace
** BLOCKED Port credential store
   Blocker: waiting on the API key
```

Checkbox items (`- [ ]`, `- [X]`, `- [-]` in progress) are read too.
