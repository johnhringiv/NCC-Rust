# Makefile for Rust project quality checks (now fully stable)

.PHONY: fmt sort check clippy test udeps quality fix ensure-tools

# -----------------------------------------------
# Install needed tools if missing
# -----------------------------------------------
ensure-tools:
	@command -v cargo-sort >/dev/null || cargo install cargo-sort
	@command -v cargo-udeps >/dev/null || cargo install cargo-udeps

# -----------------------------------------------
# Check-only tasks
# -----------------------------------------------
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

udeps: ensure-tools
	cargo udeps --all-targets --workspace --benches

# -----------------------------------------------
# Fix-only tasks
# -----------------------------------------------
fix-fmt:
	cargo fmt --all

fix-sort: ensure-tools
	cargo sort --workspace

# -----------------------------------------------
# Aggregates
# -----------------------------------------------
quality: fmt sort check clippy test udeps
	@echo "✓ All quality checks passed"

fix: fix-fmt fix-sort
	@echo "✓ Auto-fixes applied: formatting + dependency order"
