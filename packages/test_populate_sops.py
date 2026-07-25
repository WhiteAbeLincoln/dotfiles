import importlib.util
import sys
import unittest
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
        self.edits = 0

    def has(self, name):
        return name in self.values

    def get(self, name):
        return self.values[name]

    def set(self, name, value):
        self.values[name] = value

    def edit(self):
        self.edits += 1


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
            edit=False,
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
            edit=False,
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
            edit=False,
            generate=lambda _: "new",
            derive=lambda value: f"hash:{value}",
        )

        self.assertEqual(store.values["client"], "new")
        self.assertEqual(store.values["client_hash"], "hash:new")

    def test_empty_fixed_value_fails_after_editor(self):
        store = MemoryStore()
        fields = [
            Field(
                name="api_key",
                mode="fixed",
                tags=frozenset(),
                description="operator key",
            )
        ]

        with self.assertRaisesRegex(
            populate_sops.PopulateError, "operator-managed fields are empty"
        ):
            populate_sops.populate(
                fields,
                store,
                force=False,
                edit=True,
                derive=lambda value: value,
            )

        self.assertEqual(store.edits, 1)


if __name__ == "__main__":
    unittest.main()
