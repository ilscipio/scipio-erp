---
name: scipio-example-skill
description: One sentence. State the task this skill covers and when an agent should read it.
metadata:
  scipio-server: example
---

# Scipio example skill

Replace this paragraph. Name the MCP server this skill supports (the `scipio-server` value above),
and list the tools it exposes in one or two sentences.

## Procedure

1. Call `scipio_whoami`. State which permission the task needs, for example `EXAMPLE_VIEW` or
   `EXAMPLE_UPDATE`.
2. Call the dedicated find tool, for example `example_find`, to locate the record. Keep `limit`
   small.
3. Call the dedicated get tool, for example `example_get`, with one id to read the full record.
4. For a task with no dedicated tool, call `scipio_search_services` with two or three words that
   describe the goal, then follow the general service procedure in the `scipio-service-explorer`
   skill.
5. Run a write with `dryRun: true` first when the tool supports it. Fix any validation error before
   the real run.

## <Replace with a domain section, for example "Status flow" or "Creating a record">

Describe one domain rule in short sentences. Use a numbered list for a sequence, and a plain list
for a set of options. State the exact field names and status ids the agent will see.

## Conventions

- State the id format (usually a string).
- State the money format (a string with two decimals, for example `"19.99"`).
- State the date format (ISO-8601, for example `2026-01-31T10:00:00Z`).

## Safety

- Name the tools that change data. State the fields the agent must confirm with the user before a
  write.
- State any action the agent must never take without an explicit user instruction.
- Remind the agent that a permission error means the token user lacks a security group, not that
  the tool is broken.
