# AGENTS.md

## Build/Sync Commands
- `doom sync`, `doom install`, `doom update`

## Lint/Format Commands
- Byte-compile Elisp: `emacs --batch -Q -l ~/.doom.d/init.el --batch -f batch-byte-compile`
- Check config health: `doom doctor`

## Testing Commands
- No ERT tests defined; add tests under tests/
- Run a single ERT test: `emacs --batch -l ert -l tests/FILE.el -f ert-run-tests-batch-and-exit`

## Code Style Guidelines
- Indent with 2 spaces; max line length 80
- Use `use-package` for package declarations
- Group settings in init.el, config.el, packages.el
- Group `require` calls at top of files
- Document functions with docstrings
- Prefix private vars with `+` for consistency
- Use `condition-case` for error handling in config blocks

## Cursor/Copilot Rules
- No .cursor/rules directory detected
- No .cursorrules file detected
- No Copilot instructions file detected
