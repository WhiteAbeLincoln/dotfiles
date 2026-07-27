import importlib.util
import io
import sys
import unittest
from contextlib import redirect_stdout
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("populate-sops.py")
SPEC = importlib.util.spec_from_file_location("populate_sops", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
populate_sops = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = populate_sops
SPEC.loader.exec_module(populate_sops)

Field = populate_sops.Field


class MemoryStore:
    def __init__(self, values=None):
        self.values = dict(values or {})

    def has(self, name):
        return name in self.values

    def get(self, name):
        return self.values[name]

    def set(self, name, value):
        self.values[name] = value


def generated(name, *tags):
    return Field(
        name=name,
        mode="generated",
        tags=frozenset(tags),
        generator="token_urlsafe",
        byte_count=32,
    )


def derived(name, source, *tags):
    return Field(
        name=name,
        mode="derived",
        tags=frozenset(tags),
        source=source,
        derivation="authelia_argon2",
    )


class SelectionTests(unittest.TestCase):
    def test_selecting_source_keeps_derived_value_consistent(self):
        fields = [
            generated("client", "app"),
            derived("client_hash", "client", "app"),
            generated("unrelated", "other"),
        ]

        selected = populate_sops.select_fields(fields, ["client"], [])

        self.assertEqual([field.name for field in selected], ["client", "client_hash"])

    def test_tag_filter_selects_only_matching_application(self):
        fields = [
            generated("immich", "immich"),
            derived("immich_hash", "immich", "immich"),
            generated("abs", "abs"),
        ]

        selected = populate_sops.select_fields(fields, [], ["immich"])

        self.assertEqual([field.name for field in selected], ["immich", "immich_hash"])


class PopulationTests(unittest.TestCase):
    def test_population_prints_readable_field_blocks(self):
        store = MemoryStore()
        fields = [
            Field(
                name="client",
                mode="generated",
                tags=frozenset(),
                description="OIDC client credential",
                generator="token_urlsafe",
                byte_count=32,
            ),
            Field(
                name="client_hash",
                mode="derived",
                tags=frozenset(),
                description="Authelia client credential digest",
                source="client",
                derivation="authelia_argon2",
            ),
        ]

        output = io.StringIO()
        with redirect_stdout(output):
            populate_sops.populate(
                fields,
                store,
                force=False,
                generate=lambda _: "random",
                derive=lambda value: f"hash:{value}",
            )

        self.assertEqual(
            output.getvalue(),
            "\n".join(
                [
                    "client",
                    "OIDC client credential",
                    "Generated 32 bytes by token_urlsafe",
                    "",
                    "client_hash",
                    "Authelia client credential digest",
                    "Derived from client by authelia_argon2",
                    "",
                    "",
                ]
            ),
        )

    def test_authelia_derivation_produces_a_verifiable_argon2id_hash(self):
        from argon2 import PasswordHasher

        digest = populate_sops.authelia_argon2("client-secret")

        self.assertTrue(PasswordHasher().verify(digest, "client-secret"))

    def test_new_secret_and_hash_are_written_as_a_pair(self):
        store = MemoryStore()
        fields = [generated("client"), derived("client_hash", "client")]

        populate_sops.populate(
            fields,
            store,
            force=False,
            generate=lambda _: "random",
            derive=lambda value: f"hash:{value}",
        )

        self.assertEqual(
            store.values,
            {"client": "random", "client_hash": "hash:random"},
        )

    def test_existing_values_are_preserved_without_force(self):
        store = MemoryStore({"client": "old", "client_hash": "hash:old"})
        fields = [generated("client"), derived("client_hash", "client")]

        changed = populate_sops.populate(
            fields,
            store,
            force=False,
            generate=lambda _: "new",
            derive=lambda value: f"hash:{value}",
        )

        self.assertEqual(changed, [])
        self.assertEqual(store.values["client"], "old")
        self.assertEqual(store.values["client_hash"], "hash:old")

    def test_force_regenerates_secret_and_matching_hash(self):
        store = MemoryStore({"client": "old", "client_hash": "hash:old"})
        fields = [generated("client"), derived("client_hash", "client")]

        populate_sops.populate(
            fields,
            store,
            force=True,
            generate=lambda _: "new",
            derive=lambda value: f"hash:{value}",
        )

        self.assertEqual(store.values["client"], "new")
        self.assertEqual(store.values["client_hash"], "hash:new")

    def test_fixed_field_prompt_replaces_existing_value_and_retries_empty_input(self):
        store = MemoryStore({"api_key": "old-secret"})
        fields = [
            Field(
                name="api_key",
                mode="fixed",
                tags=frozenset(),
                description="operator key",
            )
        ]
        responses = iter(["", "new-secret"])
        prompts = []

        def prompt(label):
            prompts.append(label)
            return next(responses)

        output = io.StringIO()
        with redirect_stdout(output):
            populate_sops.populate(
                fields,
                store,
                force=False,
                derive=lambda value: value,
                prompt=prompt,
            )

        self.assertEqual(
            prompts,
            [
                "Enter value: ",
                "Enter value: ",
            ],
        )
        self.assertEqual(store.values["api_key"], "new-secret")
        self.assertTrue(
            output.getvalue().startswith("api_key\noperator key\n")
        )
        self.assertIn("Value cannot be empty.", output.getvalue())
        self.assertNotIn("new-secret", output.getvalue())


if __name__ == "__main__":
    unittest.main()
