# Makefile for Rust project quality checks (stable toolchain only)

.PHONY: fmt sort check clippy test machete quality fix ensure-tools

# ------------------------------------------------
# Auto-install helper tools if missing
# ------------------------------------------------
ensure-tools:
	@command -v cargo-sort >/dev/null   || cargo install cargo-sort
	@command -v cargo-machete >/dev/null || cargo install cargo-machete

# ------------------------------------------------
# Check-only tasks
# ------------------------------------------------
fmt:
	cargo fmt --all -- --check

sort: ensure-tools
	cargo sort --workspace --check

check:
	cargo check --workspace --all-features

clippy:
	cargo clippy --workspace --all-targets --all-features -- -D warnings

test:
	cargo test --workspace --all-features --no-fail-fast

machete: ensure-tools
	# exit-code 0 = OK, 1 = unused deps found → fail CI
	cargo machete --with-metadata

# ------------------------------------------------
# Fix-only tasks
# ------------------------------------------------
fix-fmt:
	cargo fmt --all

fix-sort: ensure-tools
	cargo sort --workspace

# ------------------------------------------------
# Aggregates
# ------------------------------------------------
quality: fmt sort check clippy test machete
	@echo "✓ All quality checks passed"

fix: fix-fmt fix-sort
	@echo "✓ Auto-fixes applied (format + dep order)"
