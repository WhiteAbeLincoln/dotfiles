# Python

- Prefer `uv` + `pyproject.toml` (or PEP 723 inline script metadata) over `pip` + `requirements.txt` in all cases. For single-file scripts, embed dependencies with PEP 723 and run via `uv run script.py` (or shebang `#!/usr/bin/env -S uv run --script`); for projects, use `pyproject.toml` with `uv sync` / `uv run`.
