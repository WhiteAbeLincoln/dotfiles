#!/usr/bin/env python3
"""Populate a sops file from a reusable TOML field schema."""

from __future__ import annotations

import argparse
import fnmatch
import getpass
import json
import os
import secrets
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Protocol, Sequence


class PopulateError(Exception):
    """An expected schema, selection, or external-command error."""


@dataclass(frozen=True)
class Field:
    name: str
    mode: str
    tags: frozenset[str]
    description: str | None = None
    generator: str | None = None
    byte_count: int | None = None
    source: str | None = None
    derivation: str | None = None


class SecretStore(Protocol):
    def has(self, name: str) -> bool: ...

    def get(self, name: str) -> str: ...

    def set(self, name: str, value: str) -> None: ...


class SopsStore:
    def __init__(
        self, path: Path, run: Callable[..., subprocess.CompletedProcess[str]]
    ):
        self.path = path
        self.run = run

    @staticmethod
    def _selector(name: str) -> str:
        return json.dumps([name], separators=(",", ":"))

    def has(self, name: str) -> bool:
        result = self.run(
            ["sops", "-d", "--extract", self._selector(name), str(self.path)],
            text=True,
            capture_output=True,
            check=False,
        )
        return result.returncode == 0

    def get(self, name: str) -> str:
        result = self.run(
            ["sops", "-d", "--extract", self._selector(name), str(self.path)],
            text=True,
            capture_output=True,
            check=False,
        )
        if result.returncode != 0:
            raise PopulateError(f"could not read {name!r} from {self.path}")
        return result.stdout.rstrip("\n")

    def set(self, name: str, value: str) -> None:
        self.run(
            [
                "sops",
                "set",
                str(self.path),
                self._selector(name),
                json.dumps(value),
            ],
            text=True,
            check=True,
        )


def parse_schema(path: Path) -> list[Field]:
    try:
        raw = tomllib.loads(path.read_text())
    except (OSError, tomllib.TOMLDecodeError) as error:
        raise PopulateError(f"could not read schema {path}: {error}") from error

    if raw.get("version") != 1:
        raise PopulateError("schema must declare version = 1")
    raw_fields = raw.get("fields")
    if not isinstance(raw_fields, list) or not raw_fields:
        raise PopulateError("schema must contain at least one [[fields]] entry")

    fields: list[Field] = []
    names: set[str] = set()
    for index, item in enumerate(raw_fields, start=1):
        if not isinstance(item, dict):
            raise PopulateError(f"field #{index} must be a table")
        name = item.get("name")
        mode = item.get("mode")
        if not isinstance(name, str) or not name:
            raise PopulateError(f"field #{index} has an invalid name")
        if name in names:
            raise PopulateError(f"duplicate field name: {name}")
        if mode not in {"generated", "derived", "fixed"}:
            raise PopulateError(f"{name}: mode must be generated, derived, or fixed")

        tags = item.get("tags", [])
        if not isinstance(tags, list) or not all(isinstance(tag, str) for tag in tags):
            raise PopulateError(f"{name}: tags must be an array of strings")

        field = Field(
            name=name,
            mode=mode,
            tags=frozenset(tags),
            description=item.get("description"),
            generator=item.get("generator"),
            byte_count=item.get("bytes"),
            source=item.get("source"),
            derivation=item.get("derivation"),
        )
        if mode == "generated":
            if field.generator != "token_urlsafe":
                raise PopulateError(
                    f"{name}: unsupported generator {field.generator!r}"
                )
            if not isinstance(field.byte_count, int) or field.byte_count < 16:
                raise PopulateError(f"{name}: bytes must be an integer of at least 16")
        elif mode == "derived":
            if field.derivation != "authelia_argon2":
                raise PopulateError(
                    f"{name}: unsupported derivation {field.derivation!r}"
                )
            if not isinstance(field.source, str) or not field.source:
                raise PopulateError(f"{name}: derived fields require a source")

        fields.append(field)
        names.add(name)

    for field in fields:
        if field.mode == "derived" and field.source not in names:
            raise PopulateError(f"{field.name}: unknown source {field.source!r}")
    return fields


def select_fields(
    fields: Sequence[Field],
    patterns: Sequence[str],
    tags: Sequence[str],
) -> list[Field]:
    if not patterns and not tags:
        selected_names = {field.name for field in fields}
    else:
        selected_names = {
            field.name
            for field in fields
            if any(fnmatch.fnmatchcase(field.name, pattern) for pattern in patterns)
            or any(tag in field.tags for tag in tags)
        }
        if not selected_names:
            raise PopulateError("filters did not match any fields")

    # A selected derived value needs its source. A selected source also selects
    # its direct derived values so regenerating a secret cannot leave stale hashes.
    changed = True
    while changed:
        changed = False
        for field in fields:
            if field.mode != "derived" or field.source is None:
                continue
            if field.name in selected_names and field.source not in selected_names:
                selected_names.add(field.source)
                changed = True
            if field.source in selected_names and field.name not in selected_names:
                selected_names.add(field.name)
                changed = True

    return [field for field in fields if field.name in selected_names]


def authelia_argon2(value: str) -> str:
    from argon2.low_level import Type, hash_secret

    digest = hash_secret(
        secret=value.encode(),
        salt=os.urandom(16),
        time_cost=3,
        memory_cost=65536,
        parallelism=4,
        hash_len=32,
        type=Type.ID,
    )
    return digest.decode()


def print_field_header(field: Field) -> None:
    print(field.name)
    if field.description:
        print(field.description)


def populate(
    fields: Sequence[Field],
    store: SecretStore,
    *,
    force: bool,
    generate: Callable[[int], str] = secrets.token_urlsafe,
    derive: Callable[[str], str],
    prompt: Callable[[str], str] = getpass.getpass,
) -> list[str]:
    changed: list[str] = []
    selected_names = {field.name for field in fields}

    for field in fields:
        if field.mode != "generated":
            continue
        print_field_header(field)
        exists = store.has(field.name)
        if exists and not force:
            print("Keeping existing value")
            print()
            continue
        assert field.byte_count is not None
        assert field.generator is not None
        store.set(field.name, generate(field.byte_count))
        changed.append(field.name)
        print(f"Generated {field.byte_count} bytes by {field.generator}")
        print()

    for field in fields:
        if field.mode != "derived":
            continue
        print_field_header(field)
        assert field.source is not None
        assert field.derivation is not None
        source_changed = field.source in changed
        exists = store.has(field.name)
        if exists and not force and not source_changed:
            print("Keeping existing value")
            print()
            continue
        if field.source not in selected_names and not store.has(field.source):
            raise PopulateError(f"{field.name}: source {field.source!r} is absent")
        source_value = store.get(field.source)
        store.set(field.name, derive(source_value))
        changed.append(field.name)
        print(f"Derived from {field.source} by {field.derivation}")
        print()

    fixed_fields = [field for field in fields if field.mode == "fixed"]
    for field in fixed_fields:
        print_field_header(field)
        value = prompt("Enter value: ")
        while not value:
            print("Value cannot be empty.")
            value = prompt("Enter value: ")
        store.set(field.name, value)
        changed.append(field.name)
        print()
    return changed


def make_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="populate-sops",
        description="Populate a sops file from a TOML field schema.",
    )
    parser.add_argument("schema", type=Path)
    parser.add_argument("secrets_file", type=Path)
    parser.add_argument(
        "--field",
        action="append",
        default=[],
        metavar="GLOB",
        help="select field names matching a glob; repeatable",
    )
    parser.add_argument(
        "--tag",
        action="append",
        default=[],
        help="select fields carrying a tag; repeatable",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="regenerate selected generated and derived fields",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = make_parser().parse_args(argv)
    try:
        fields = parse_schema(args.schema)
        selected = select_fields(fields, args.field, args.tag)
        store = SopsStore(args.secrets_file, subprocess.run)
        populate(
            selected,
            store,
            force=args.force,
            derive=authelia_argon2,
        )
    except (PopulateError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
