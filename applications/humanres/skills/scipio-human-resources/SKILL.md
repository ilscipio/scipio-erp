---
name: scipio-human-resources
description: How to find and read employees and positions, create an employee, and create a leave request through the Scipio MCP humanres tools. Use for any HR request.
metadata:
  scipio-server: humanres
---

# Scipio human resources

The `humanres` MCP server (`/humanres/mcp`) manages employees, employment, positions, leave and
reviews. Personal data is involved; keep every answer to the minimum the user needs.

## Procedure: find an employee

1. Call `scipio_whoami`. Confirm `HUMANRES_VIEW` for reads and `HUMANRES_UPDATE` for writes.
2. Call `employee` with action `find` with `firstName`, `lastName`, `partyId` or `employerPartyId`.
3. Call `employee` with action `get` with the `partyId` to read employments, positions, skills and leave.

## Procedure: create an employee

1. Call `employee` with action `find` first to avoid a duplicate.
2. Call `employee` with action `create` with `firstName`, `lastName`, `partyIdFrom` (the employer organization,
   for example `Company`), `fromDate`, and optional address and email fields. Keep the returned
   `partyId`.
3. Call `position` with action `find` with `partyId` = the organization and `statusId` = `EMPL_POS_ACTIVE` to
   find an open position. Create one with `position` with action `create` when none exists.
4. Fill the position through the service catalog (call `scipio_service` with action `search` with
   `position fulfillment`).

## Procedure: leave request

1. Call `employee` with action `get` to read existing `leave` rows and avoid an overlap.
2. Call `employee` with action `leave_create` with `partyId`, `leaveTypeId` (for example `VACATION`, `SICK_LEAVE`),
   `fromDate`, `thruDate` and `description`. The request starts as `LEAVE_CREATED`.
3. Approval goes through the service catalog (`updateEmplLeaveStatus`) by a user with the right
   permission. Do not approve a request yourself.

## Reads

- `employee` actions `find` and `get`, `position` action `find` as above.
- Reviews, skills, benefits: call `scipio_service` with action `search` with the matching words.

## Conventions

- Dates are ISO-8601 or `yyyy-MM-dd`.
- An employer is a party group; an employee is a person with the `EMPLOYEE` role.
- Ids are strings.

## Safety

- Do not disclose salary, review or leave data beyond what the user asked for.
- Do not end an employment or a position without an explicit user instruction.
- A permission error means the token user lacks `HUMANRES_UPDATE`. Report it.
