.PHONY: sync-agent-docs check-agent-docs

sync-agent-docs:
	./scripts/sync-agent-docs.sh

check-agent-docs:
	./scripts/sync-agent-docs.sh --check
