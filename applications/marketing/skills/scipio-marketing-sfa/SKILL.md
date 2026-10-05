---
name: scipio-marketing-sfa
description: How to find and create leads, find, create and update sales opportunities, and find campaigns through the Scipio MCP marketing tools. Use for any sales pipeline or campaign task.
metadata:
  scipio-server: marketing
---

# Scipio marketing and sales force automation

The `marketing` MCP server (`/marketing/mcp` and `/sfa/mcp`) covers leads, opportunities,
campaigns, contact lists, segments and tracking codes.

## Procedure: qualify a lead into an opportunity

1. Call `scipio_whoami`. Confirm `MARKETING_VIEW` for reads and `MARKETING_UPDATE` for writes.
2. Call `sfa` with action `lead_find` with `firstName`, `lastName` or `groupName` to check for an existing lead.
3. Call `sfa` with action `lead_create` with `firstName`, `lastName`, `groupName` (company), `emailAddress` and a
   phone number when known. Keep the returned party id of the lead.
4. Call `sfa` with action `opportunity_create` with `opportunityName`, `estimatedAmount`, `currencyUomId`,
   `estimatedCloseDate`, `opportunityStageId` (start with `SOSTG_PROSPECT`) and `leadPartyId`.
   Keep the returned `salesOpportunityId`.
5. As the deal moves, call `sfa` with action `opportunity_update` with `salesOpportunityId` and the new
   `opportunityStageId`, `estimatedAmount` or `estimatedProbability`.
6. When the lead becomes a customer, call `sfa` with action `lead_convert` with the lead party id. Confirm with the
   user first.

## Reads

- `sfa` action `opportunity_find`: by `salesOpportunityId`, `opportunityStageId`, `nameLike` or `partyId`.
- `campaign` action `find`: by `marketingCampaignId`, `statusId` or `nameLike`.
- Contact lists, segments, tracking codes, forecasts: call `scipio_service` with action `search` with the matching
  words.

## Stage ids

- `SOSTG_PROSPECT`, `SOSTG_QUALIFICATION`, `SOSTG_NEEDS_ANALYSIS`, `SOSTG_VALUE_PROPOSITION`,
  `SOSTG_PROPOSAL`, `SOSTG_NEGOTIATION`, `SOSTG_CLOSED_WON`, `SOSTG_CLOSED_LOST`.

## Conventions

- Money is a string with two decimals; `estimatedProbability` is a number from 0 to 100.
- Dates are ISO-8601.
- A lead, a contact and an account are parties with the `LEAD`, `CONTACT` and `ACCOUNT` roles.

## Safety

- `campaign` action `create` and `sfa` action `lead_convert` create records other users work with. Confirm the names and
  the amounts with the user before you call them.
- Do not close an opportunity as won or lost without an explicit user instruction.
- A permission error means the token user lacks `MARKETING_UPDATE`. Report it.
