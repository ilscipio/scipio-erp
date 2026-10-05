---
name: scipio-work-management
description: How to find, create, update and assign tasks, events and projects through the Scipio MCP workeffort tools. Use for any task, calendar or project request.
metadata:
  scipio-server: workeffort
---

# Scipio work management

The `workeffort` MCP server (`/workeffort/mcp`) manages work efforts: tasks, events, projects,
milestones and their party assignments. Timesheets and deliverables are reachable through the
service catalog.

## Procedure: create and assign a task

1. Call `scipio_whoami`. Confirm `WORKEFFORTMGR_VIEW` for reads and `WORKEFFORTMGR_UPDATE` for writes.
2. Call `task` with action `find` with `nameLike`, `workEffortTypeId` or `partyId` to check for an existing task.
3. Call `task` with action `create` with `workEffortTypeId` (`TASK`, `EVENT`, `PROJECT` or `MILESTONE`),
   `workEffortName`, `currentStatusId` (`CAL_NEEDS_ACTION` for a new task), `estimatedStartDate`,
   `estimatedCompletionDate` and `description`. Keep the returned `workEffortId`.
4. Call `task` with action `assign` with `workEffortId`, `partyId`, `roleTypeId` (`CAL_OWNER` for the
   responsible person, `CAL_ATTENDEE` for others) and `statusId` = `PRTYASGN_ASSIGNED`.
5. Call `task` with action `get` with the `workEffortId` and confirm the assignment appears.

## Procedure: update status

1. Call `task` with action `get` to read the current `currentStatusId`.
2. Call `task` with action `set_status` with `workEffortId` and the new `currentStatusId`: `CAL_ACCEPTED` (started),
   `CAL_COMPLETED` (done), `CAL_CANCELLED` (stopped), `CAL_DECLINED`.
3. Call `task` with action `update` to change dates, name, description or `priority`.

## Reads

- `task` action `find`: by `workEffortId`, `workEffortTypeId`, `currentStatusId`, assigned `partyId`,
  `nameLike` or a planned date range. Rows are newest first.
- `task` action `get`: the work effort with assignments, child work efforts, associations and attributes.
- A project's tasks are the children of the project; find them with `task` with action `get` on the project.

## Conventions

- Dates are ISO-8601, for example `2026-03-01T09:00:00Z`.
- `priority` is a number; 1 is the highest.
- A work effort id is a string such as `10001`.

## Safety

- Do not cancel or complete a task without an explicit user instruction.
- Do not assign a party that the user did not name.
- A permission error means the token user lacks `WORKEFFORTMGR_UPDATE`. Report it.
