---
name: scipio-customer-management
description: How to find a party, read its detail, create a person or a customer, and add a contact mechanism or a role through the Scipio MCP party tools. Use for any customer task.
metadata:
  scipio-server: party
---

# Scipio customer management

The `party` MCP server covers people, groups, roles, and contact information. It exposes dedicated
tools for lookup and falls back to `scipio_service` with action `search` for creation and change.

## Procedure

1. Call `scipio_whoami`. Confirm the user holds `PARTYMGR_VIEW` at least, and `PARTYMGR_UPDATE` for
   any create or change.
2. Call `party` with action `find` to locate a party. Pass a name, an id, a role, or a contact value such as an
   email address. Keep `limit` small.
3. Call `party` with action `get` with one `partyId` to read the full record: type, roles, contact mechanisms,
   and status.
4. To create a new customer, call `party` with action `create_customer` with `firstName`, `lastName` and, when known,
   `emailAddress`, `contactNumber`, `countryCode`, `address1`, `city`, `postalCode`,
   `countryGeoId` and `stateProvinceGeoId`. The tool creates the person, the `CUSTOMER` role and
   the contact mechanisms (the address doubles as shipping and billing location). Keep the
   returned `partyId`.
5. To add contact information to an existing party, call `party` with action `contact_add` with `partyId`,
   `type` (`EMAIL_ADDRESS`, `TELECOM_NUMBER` or `POSTAL_ADDRESS`), a `purpose` (for example
   `SHIPPING_LOCATION`) and the matching fields.
6. For a role, a relationship or a note, search and run the matching service through
   `scipio_service` with actions `search` and `call` with `dryRun: true` first.

## Creating a person or a customer

Run `createPerson` (`applications/party/src/com/ilscipio/scipio/party/service/Services.java`) with
the person's name fields. The service returns a new `partyId`. To also grant a login, run
`createPersonAndUserLogin` instead. Neither service assigns a role by itself; add one as a separate
step when the party acts as a customer.

## Contact mechanisms

Run `createContactMech` to create the raw record (an email address, a phone number, or a postal
address), then run `createPartyContactMech` to attach it to the party, or run
`createPartyPostalAddress` or `createPartyTelecomNumber` for the combined step. State the contact
mechanism type and purpose to the user before you run a write.

## Roles

Run `createPartyRole` to add a role, such as `CUSTOMER`, to an existing party. Run
`createPartyRelationship` for a link between two parties, such as an organization and one of its
contacts.

## Party status

A party carries a `statusId`, for example `PARTY_ENABLED` or `PARTY_DISABLED`. Do not disable a
party without an explicit user instruction; a disabled party can no longer sign in or place an
order.

## Conventions

- Party ids are strings. Names are case-sensitive as entered.
- Dates are ISO-8601, for example `2026-01-31T10:00:00Z`.
- A role id such as `CUSTOMER` or `SUPPLIER` follows the standard Scipio role type list; call
  `scipio_entity` with action `describe` on `RoleType` when a role id is unclear.

## Safety

- Creating a person, a contact mechanism, or a role changes data. State the fields to the user
  before you run the write, unless the user already gave every field.
- Never create a duplicate party without checking `party` with action `find` first.
- A permission error means the token user lacks a security group. Report the missing permission.
