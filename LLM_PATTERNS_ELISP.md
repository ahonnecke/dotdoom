# LLM-Friendly Patterns: Emacs Lisp

This document governs how Emacs Lisp code in this project should be written, structured, and reasoned about. It assumes the primary reader and modifier is an LLM piloted by a human engineer.

It is a companion to `LLM_PATTERNS.md`. The general principles there apply here. This document covers what is different or intensified in Emacs Lisp specifically.

---

## The Core Problem in Emacs Lisp

Emacs Lisp has more ways to make behavior implicit than almost any other language in common use:

- Dynamic binding means a function's behavior depends on what the caller happened to have bound
- The advice system allows any function to be silently wrapped from anywhere in the codebase
- Buffer-local variables mean the same symbol holds different values depending on which buffer is current
- Hooks distribute behavior across many files with no central registry

Every one of these is a place where an LLM reading the code cannot determine correct behavior without tracing runtime state. The design goal of this codebase is to eliminate or explicitly document all such sites.

---

## Non-Negotiable: Lexical Binding

Every file in this project must declare lexical binding:

```elisp
;;; feature-name.el --- Short description -*- lexical-binding: t -*-
```

This is the single most impactful change for LLM-friendliness in an Emacs Lisp codebase. With dynamic binding (the default), the value of any free variable in a function depends on the call stack at runtime — the LLM cannot determine it by reading the code. With lexical binding, variable scope is determinable statically.

There are no exceptions to this rule. If a construct requires dynamic binding, use `defvar` to declare it explicitly as a dynamic variable and document it as such.

---

## Docstrings Are the Type System

In TypeScript, the type system carries the contract. In Emacs Lisp there is no type system, so **docstrings carry the entire load**. They are not courtesy — they are the only machine-readable contract a function has.

Every `defun`, `defvar`, `defcustom`, and `cl-defstruct` must have a docstring. The docstring must specify:

- What each argument is, what type is expected, and what `nil` means if it is allowed
- What the function returns, including what it returns on failure or edge cases
- Any side effects — buffer modifications, variable mutations, I/O
- Any preconditions: what must be true about the environment for the function to behave correctly

```elisp
;; Bad: The LLM must guess what this accepts and what it returns
(defun process-region (start end flags)
  "Process region."
  ...)

;; Good: The contract is fully stated
(defun process-region (start end flags)
  "Process the region from START to END according to FLAGS.

START and END are buffer positions (integers or markers).
FLAGS is a plist accepting the following keys:
  :dry-run  If non-nil, analyze but do not modify the buffer.
  :verbose  If non-nil, message progress to *Messages*.

Returns a plist with keys :changes (integer count of modifications made)
and :errors (list of strings describing any skipped regions).
Returns nil if the region is empty."
  ...)
```

The `(interactive)` spec is also a contract. Use it fully and precisely. `(interactive "r")` tells the LLM this function operates on the active region. That is load-bearing information.

---

## Structured Data: Use `cl-defstruct`

A plist like `(:name "foo" :amount 42)` has no schema an LLM can read. An alist is worse. A `cl-defstruct` names the type, names the slots, gives them default values, and generates accessor functions that are statically findable.

```elisp
;; Bad: implicit schema, no type information, LLM must infer slot names from usage
(defun make-payment (amount currency)
  (list :amount amount :currency currency :status 'pending))

;; Good: explicit schema, typed accessors, readable at definition site
(cl-defstruct (payment (:constructor make-payment))
  "A payment pending processing.
AMOUNT is a float representing the charge in the smallest currency unit.
CURRENCY is a symbol, one of: usd eur gbp.
STATUS is one of: pending charged refunded failed."
  (amount   nil :type float)
  (currency nil :type symbol)
  (status   'pending :type symbol))
```

Use `cl-defstruct` for any data that crosses a function boundary or is stored in a variable that outlives a single call. Reserve plists for transient local data within a single function.

---

## The Advice System

`advice-add` and `defadvice` allow any function to be silently wrapped from anywhere. An LLM modifying a function cannot know it has been advised elsewhere. An LLM generating a call to a function cannot know its behavior has been altered.

**Treat every use of the advice system as a hazard site.**

Rules:

- Never use advice to implement core feature behavior. Advice is for integration points between packages, compatibility shims, and instrumentation only.
- Every call to `advice-add` must have a comment stating: what function is being advised, why direct modification was not possible, and what the advice does.
- All advice in this project must be co-located in a single file: `advice.el`. It must not be scattered across feature files.

```elisp
;; advice.el

;; We advise `save-buffer` here rather than modifying it directly because it is
;; a built-in. This advice triggers our format-on-save pipeline.
;; See: formatting.el#run-format-pipeline
(advice-add 'save-buffer :before #'project--format-before-save)
```

If you are reading code that calls a function and its behavior seems inconsistent with its definition, check `advice.el` before concluding there is a bug.

---

## Hooks

Hooks are a distributed observer pattern. Behavior registered via `add-hook` executes implicitly, from potentially many distant files, with no central registry. An LLM modifying behavior around a hook cannot know what else will run.

**Maintain a hook registry.** The file `hooks.el` must list every hook this project adds to, in the following format:

```elisp
;; hooks.el — Registry of all hooks used by this project.
;;
;; Format:
;;   HOOK-NAME
;;     Function: the function added
;;     File:     where the function is defined
;;     Purpose:  what it does and why this hook
;;     State:    what buffer-local or global state it depends on
;;
;; after-save-hook
;;   Function: project--sync-index-on-save
;;   File:     index.el
;;   Purpose:  Updates the search index when a tracked file is saved.
;;             Uses after-save rather than before-save because the index
;;             must reflect the committed buffer content.
;;   State:    Depends on `project-tracked-directories` being set.
```

Every `add-hook` call in this project must have a corresponding entry in `hooks.el`. This file is not generated — it is maintained by hand and reviewed on every change to hook registrations.

---

## Buffer-Local Variables

Buffer-local variables mean the same symbol holds different values depending on which buffer is current. This is invisible to static reading.

Rules:

- Every `make-local-variable` and `setq-local` must have a comment stating which buffer context is expected and why the variable must be local rather than global.
- Functions that depend on a buffer-local variable being set must state this in their docstring as a precondition.
- Never read a variable that might be buffer-local without being explicit about which buffer you intend: use `with-current-buffer` rather than relying on the implicit current buffer.

```elisp
;; Bad: relies on implicit current buffer; LLM cannot determine which buffer's
;; value of project-mode-state will be read
(defun project--check-state ()
  (when project-mode-state
    ...))

;; Good: explicit about buffer context
(defun project--check-state (buffer)
  "Check project state for BUFFER.
Requires that `project-mode-state' is set buffer-locally in BUFFER,
which is guaranteed when `project-mode' is active."
  (with-current-buffer buffer
    (when project-mode-state
      ...)))
```

---

## Macros

Macros are less dangerous for LLMs than they first appear, because homoiconicity means an LLM can reason about macro expansion as a data transformation. Standard Emacs macros (`with-current-buffer`, `save-excursion`, `cl-loop`, etc.) are reliably understood.

Custom macros in a project-specific codebase are the risk. An LLM will attempt to use them based on their name alone, without knowing their expansion behavior.

Rules:

- Every custom macro must have a docstring that describes its expansion, not just its intent.
- Prefer functions over macros when a function is sufficient. Macros are justified when the use site benefits from delayed evaluation or syntactic transformation. Document which of these is the reason.
- Where a macro produces a nontrivial expansion, include an example expansion in the docstring.

```elisp
(defmacro project-with-locked-index (&rest body)
  "Execute BODY with the project index locked for exclusive write access.

Acquires `project--index-lock' before BODY and releases it after,
even if BODY signals an error. If the lock cannot be acquired within
`project-lock-timeout' seconds, signals `project-lock-timeout-error'.

Expands approximately to:
  (project--acquire-lock)
  (unwind-protect
    (progn BODY)
    (project--release-lock))"
  `(progn
     (project--acquire-lock)
     (unwind-protect
         (progn ,@body)
       (project--release-lock))))
```

---

## Package Structure

Organize by domain ownership, not by technical role. Everything belonging to a feature lives together.

```
;; Prefer this:
project-search.el       ; search feature: data, logic, UI commands
project-search-test.el

project-index.el        ; index feature: data, logic, UI commands
project-index-test.el

advice.el               ; all advice, centralized
hooks.el                ; hook registry, centralized

;; Over this:
commands.el             ; all interactive commands from all features
ui.el                   ; all UI logic from all features
data.el                 ; all data structures from all features
```

The exception is `advice.el` and `hooks.el`, which are explicitly centralized because their contents need to be auditable as a whole.

---

## Interactive Commands

Interactive commands are public API. They are the primary surface an LLM is asked to extend or modify.

- Every interactive command must have a complete docstring including its keybinding if it has one, what it operates on (region, buffer, point), and what it does when called with a prefix argument.
- Group related commands in the same file as the feature they belong to, not in a global `commands.el`.
- Use a consistent naming prefix for all commands in this project: `project-`. The LLM should be able to discover all commands in the project by searching for this prefix.

---

## Checklist

Before committing code or accepting an LLM-generated change, verify:

- [ ] Does every file declare `;;; -*- lexical-binding: t -*-`?
- [ ] Does every `defun` have a docstring that fully specifies arguments, return value, and side effects?
- [ ] Does every `cl-defstruct` have a docstring that specifies the meaning and expected type of each slot?
- [ ] Is every use of `advice-add` co-located in `advice.el` with a comment explaining why?
- [ ] Is every `add-hook` call registered in `hooks.el`?
- [ ] Does every function that depends on buffer-local state declare it as a precondition and use `with-current-buffer` explicitly?
- [ ] Does every custom macro have a docstring that describes its expansion behavior?
- [ ] Is there any place where correct behavior depends on runtime call-stack state rather than something readable in the code?
