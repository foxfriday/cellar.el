# cellar.el

Manage a wine cellar from Emacs using SQLite.

## Requirements

- Emacs 29+ (built-in `sqlite` support)
- SQLite 3.44.0+ (for `concat_ws`)

## Setup

Create the database from the included schema:

```sh
sqlite3 ~/path/to/inventory.db < schema.sql
```

Then point `cellar-database` and `cellar-note-dir` to your paths:

```elisp
(setq cellar-database "~/path/to/inventory.db")
(setq cellar-note-dir "~/path/to/notes/")
```

## Usage

### Standalone commands

| Command                  | Description                                   |
|--------------------------|-----------------------------------------------|
| `cellar-browse`          | Open the cellar buffer                        |
| `cellar-add-producer`    | Add a new producer                            |
| `cellar-add-bottle`      | Add a bottle to inventory                     |
| `cellar-add-offsite`     | Add bottles to offsite (prefix arg for count) |
| `cellar-move-onsite`     | Move bottles from offsite to home             |
| `cellar-consume`         | Consume bottles from home                     |
| `cellar-update-window`   | Update a wine's drinking window               |
| `cellar-update-alcohol`  | Update a wine's alcohol percentage            |
| `cellar-add-note`        | Add a short text note to a bottle             |
| `cellar-note`            | Create a Markdown tasting note                |
| `cellar-sync-notes`      | Batch-import all tasting notes into the DB    |

### Cellar buffer keybindings

Inside `*Cellar*` (`cellar-list-mode`):

| Key     | Action                    |
|---------|---------------------------|
| `a`     | Add bottles offsite       |
| `m`     | Move bottles onsite       |
| `c`     | Consume bottles           |
| `w`     | Update drinking window    |
| `A`     | Update alcohol            |
| `n`     | Add a text note           |
| `N`     | Create a tasting note     |
| `d`     | Delete inventory entry    |
| `RET`   | Show wine details         |
| `f`     | Filter by country/region  |
| `F`     | Clear filter              |

### Tasting notes

`cellar-note` creates a Markdown file in `cellar-note-dir` with front-matter
and scoring headings:

```markdown
---
iid:
vintage:
producer:
subregion:
varietal:
size:
date:
score:
---

## Appearance (0/3)

## Aroma (0/5)

## Taste (0/5)

## Aftertaste (0/3)

## Impression (0/4)
```

Edit the `0` in each heading to record your score. On save the note is
automatically synced into the `tastings` table. Use `cellar-sync-notes` to
batch-import all notes from the directory.

