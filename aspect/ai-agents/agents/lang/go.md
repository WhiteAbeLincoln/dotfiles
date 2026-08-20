# Golang
- Always run linters after changes and fix any issues they report:
  1. `golangci-lint run --fix ./...` to check for issues.
  2. `golangci-lint fmt ./...`
- Run unit tests after changes.
- Prefer `Ginkgo` and `Gomega` for testing, unless the module is already using a different testing framework.
- Ginkgo uses the `--focus` flag to filter tests instead of `-run`: `go test ./... --gingo.focus=TestName`

## Test Assertions
- When writing tests, assert collections using Gomega matchers like `ConsistOf`, `ContainElement`, `HaveKeyWithValue` instead of casting and indexing into maps/slices. E.g.

```go
Expect(entries[0]).To(HaveKeyWithValue(
    "grpc_request",
    HaveKeyWithValue("rpc", And(
        HaveKeyWithValue("service", "my.package.MyService"),
        HaveKeyWithValue("method", "DoSomething"),
    )),
))
```

instead of:

```go
grpcReq := entries[0]["grpc_request"].(map[string]any)
rpc := grpcReq["rpc"].(map[string]any)
Expect(rpc["service"]).To(Equal("my.package.MyService"))
Expect(rpc["method"]).To(Equal("DoSomething"))
```
