/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.party.mcp;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.party.contact.ContactHelper;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.ImportDiff;

/**
 * SCIPIO: 4.0.0: MCP server profile for the party component: people, organizations, roles and contact information.
 */
@McpServer(name = "party", title = "Scipio Party Manager", component = "party",
        description = "Party management: people and organizations, their roles, contacts and suppliers.",
        featuredServices = {"createPerson", "createPartyGroup", "createPartyContactMech", "createPartyPostalAddress",
                "createPartyTelecomNumber", "createPartyEmailAddress", "createPartyRole", "findParty", "createPartyRelationship",
                "createPartyNote"},
        entities = {"Party", "Person", "PartyGroup", "PartyRole", "PartyContactMech", "ContactMech", "PostalAddress",
                "TelecomNumber", "PartyRelationship", "PartyContactMechPurpose"},
        serviceTools = {
            @McpServiceTool(service = "createPerson",
                    topic = "party",
                    name = "create_person",
                    description = "Create a person party.",
                    readOnly = false,
                    destructive = "false",
                    order = 90),
            @McpServiceTool(service = "createPartyGroup",
                    topic = "party",
                    name = "create_group",
                    description = "Create a party group (organization).",
                    readOnly = false,
                    destructive = "false",
                    order = 91),
            @McpServiceTool(service = "updatePerson", topic = "party", name = "update_person",
                    description = "Update a person's name, title or other personal fields.", readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "updatePartyGroup", topic = "party", name = "update_group",
                    description = "Update an organization's name, revenue or other group fields.", readOnly = false, destructive = "false", order = 42),
            @McpServiceTool(service = "createPartyRole", topic = "party", name = "role_add",
                    description = "Add a role to a party, e.g. CUSTOMER, SUPPLIER, EMPLOYEE.", readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "createPartyRelationshipAndRole", topic = "party", name = "relationship_add",
                    description = "Create a relationship between two parties and add both roles.", readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "createPartyNote", topic = "party", name = "note_add",
                    description = "Add a note to a party.", readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createPartyClassification", topic = "party", name = "classification_add",
                    description = "Classify a party into a classification group.", readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "createPartyIdentification", topic = "party", name = "identification_set",
                    description = "Set an external identification number on a party.", readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "updatePartyContactMech", topic = "party", name = "contact_update",
                    description = "Update a party's contact mechanism: email, phone or address.", readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createCommunicationEvent", topic = "party", name = "comm_event_create",
                    description = "Create a communication event (email, call or note) on a party.", readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createVendor", topic = "supplier", name = "vendor_info_set",
                    description = "Create vendor information for a party.", readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createPersonAndUserLogin", topic = "party", name = "user_login_create",
                    description = "Create a person and a user login for them.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 77),
            @McpServiceTool(service = "deletePartyContactMech", topic = "party", name = "contact_expire",
                    description = "Expire a party contact mechanism and its purposes.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 78)
        },
        topics = {
            @McpTopic(name = "party", title = "Parties", order = 10, featured = true,
                    description = "Parties: find, create, update, roles, contacts and identification."),
            @McpTopic(name = "supplier", title = "Suppliers", order = 20,
                    description = "Suppliers: create, set products and prices, import, vendor info.")
        })
public final class PartyMcp {

    private PartyMcp() {}

    @McpTool(topic = "party", name = "find", description = "Find parties by id, person or group name, email or role.", readOnly = true, order = 10)
    public static Object findParties(McpCallContext ctx,
            @McpParam(name = "partyId", description = "Exact party id", required = false) String partyId,
            @McpParam(name = "firstName", description = "Person first name (partial match)", required = false) String firstName,
            @McpParam(name = "lastName", description = "Person last name (partial match)", required = false) String lastName,
            @McpParam(name = "groupName", description = "Party group name (partial match)", required = false) String groupName,
            @McpParam(name = "email", description = "Email address (partial match)", required = false) String email,
            @McpParam(name = "roleTypeId", description = "e.g. CUSTOMER, SUPPLIER", required = false) String roleTypeId,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        int max = ctx.limit(limit);
        try {
            Map<String, String> names = new LinkedHashMap<>();
            List<EntityCondition> personCond = new ArrayList<>();
            if (partyId != null) personCond.add(EntityCondition.makeCondition("partyId", partyId));
            if (firstName != null) personCond.add(EntityCondition.makeCondition("firstName", EntityOperator.LIKE, "%" + firstName + "%"));
            if (lastName != null) personCond.add(EntityCondition.makeCondition("lastName", EntityOperator.LIKE, "%" + lastName + "%"));
            boolean personFilter = firstName != null || lastName != null;
            boolean groupFilter = groupName != null;
            if (!groupFilter || personFilter) {
                for (GenericValue p : EntityQuery.use(delegator).from("Person").where(personCond).maxRows(max).queryList()) {
                    String fn = p.getString("firstName");
                    String ln = p.getString("lastName");
                    names.put(p.getString("partyId"), ((fn != null ? fn : "") + " " + (ln != null ? ln : "")).trim());
                }
            }
            if (!personFilter) {
                List<EntityCondition> groupCond = new ArrayList<>();
                if (partyId != null) groupCond.add(EntityCondition.makeCondition("partyId", partyId));
                if (groupName != null) groupCond.add(EntityCondition.makeCondition("groupName", EntityOperator.LIKE, "%" + groupName + "%"));
                for (GenericValue g : EntityQuery.use(delegator).from("PartyGroup").where(groupCond).maxRows(max).queryList()) {
                    names.put(g.getString("partyId"), g.getString("groupName"));
                }
            }
            if (email != null) {
                Set<String> matches = new LinkedHashSet<>();
                List<EntityCondition> cmCond = new ArrayList<>();
                cmCond.add(EntityCondition.makeCondition("contactMechTypeId", "EMAIL_ADDRESS"));
                cmCond.add(EntityCondition.makeCondition("infoString", EntityOperator.LIKE, "%" + email + "%"));
                for (GenericValue cm : EntityQuery.use(delegator).from("ContactMech").where(cmCond).queryList()) {
                    for (GenericValue pcm : EntityQuery.use(delegator).from("PartyContactMech")
                            .where("contactMechId", cm.getString("contactMechId")).queryList()) {
                        matches.add(pcm.getString("partyId"));
                    }
                }
                names.keySet().retainAll(matches);
            }
            if (roleTypeId != null) {
                Set<String> matches = new LinkedHashSet<>();
                for (GenericValue role : EntityQuery.use(delegator).from("PartyRole").where("roleTypeId", roleTypeId).queryList()) {
                    matches.add(role.getString("partyId"));
                }
                names.keySet().retainAll(matches);
            }
            List<Map<String, Object>> result = new ArrayList<>();
            int count = 0;
            for (Map.Entry<String, String> e : names.entrySet()) {
                if (count++ >= max) break;
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("partyId", e.getKey());
                row.put("name", e.getValue());
                result.add(row);
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Party search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "party", name = "get", description = "Get full party detail: identity, roles and contact mechanisms.", readOnly = true, order = 20)
    public static Object getParty(McpCallContext ctx,
            @McpParam(name = "partyId", description = "Party id", required = true) String partyId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue party = EntityQuery.use(delegator).from("Party").where("partyId", partyId).queryOne();
            if (party == null) {
                throw new McpToolException("Party not found: " + partyId);
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("party", ResultConverter.toJson(party));
            GenericValue person = EntityQuery.use(delegator).from("Person").where("partyId", partyId).queryOne();
            if (person != null) result.put("person", ResultConverter.toJson(person));
            GenericValue group = EntityQuery.use(delegator).from("PartyGroup").where("partyId", partyId).queryOne();
            if (group != null) result.put("group", ResultConverter.toJson(group));
            result.put("roles", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("PartyRole").where("partyId", partyId).queryList()));
            Collection<GenericValue> contactMechs = ContactHelper.getContactMech(party, false);
            result.put("contactMechs", ResultConverter.toJson(contactMechs));
            result.put("contactMechPurposes", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("PartyContactMechPurpose").where("partyId", partyId).filterByDate().queryList()));
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load party " + partyId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "party", name = "create_customer", description = "Create a customer: a person with the CUSTOMER role.", readOnly = false, destructive = "false", order = 30)
    public static Object createCustomer(McpCallContext ctx,
            @McpParam(name = "firstName", required = true) String firstName,
            @McpParam(name = "lastName", required = true) String lastName,
            @McpParam(name = "emailAddress", required = false) String emailAddress,
            @McpParam(name = "contactNumber", description = "Phone number without country code", required = false) String contactNumber,
            @McpParam(name = "countryCode", description = "Phone country code, e.g. 49", required = false) String countryCode,
            @McpParam(name = "address1", description = "Street address line 1", required = false) String address1,
            @McpParam(name = "address2", required = false) String address2,
            @McpParam(name = "city", required = false) String city,
            @McpParam(name = "postalCode", required = false) String postalCode,
            @McpParam(name = "countryGeoId", description = "e.g. DEU, USA", required = false) String countryGeoId,
            @McpParam(name = "stateProvinceGeoId", description = "e.g. CA, NY (required for some countries)", required = false) String stateProvinceGeoId,
            @McpParam(name = "externalId", description = "Id of the customer in an external system", required = false) String externalId) throws McpToolException {
        Map<String, Object> person = new LinkedHashMap<>();
        person.put("firstName", firstName);
        person.put("lastName", lastName);
        if (externalId != null) person.put("externalId", externalId);
        String partyId = (String) ctx.runService("createPerson", person).get("partyId");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("partyId", partyId);
        Map<String, Object> role = new LinkedHashMap<>();
        role.put("partyId", partyId);
        role.put("roleTypeId", "CUSTOMER");
        ctx.runService("createPartyRole", role);
        if (UtilValidate.isNotEmpty(emailAddress)) {
            out.put("emailContactMechId", addEmail(ctx, partyId, emailAddress, "PRIMARY_EMAIL"));
        }
        if (UtilValidate.isNotEmpty(contactNumber)) {
            out.put("phoneContactMechId", addPhone(ctx, partyId, countryCode, null, contactNumber, "PRIMARY_PHONE"));
        }
        if (UtilValidate.isNotEmpty(address1)) {
            String cmId = addAddress(ctx, partyId, firstName + " " + lastName, address1, address2, city, postalCode, countryGeoId,
                    stateProvinceGeoId, "GENERAL_LOCATION");
            addPurpose(ctx, partyId, cmId, "SHIPPING_LOCATION");
            addPurpose(ctx, partyId, cmId, "BILLING_LOCATION");
            out.put("postalAddressContactMechId", cmId);
        }
        return out;
    }

    @McpTool(topic = "party", name = "contact_add", description = "Add an email, phone or postal address to a party.", readOnly = false, destructive = "false", order = 40)
    public static Object addContactMech(McpCallContext ctx,
            @McpParam(name = "partyId", required = true) String partyId,
            @McpParam(name = "type", description = "Contact mech type", required = true,
                    enumValues = {"EMAIL_ADDRESS", "TELECOM_NUMBER", "POSTAL_ADDRESS"}) String type,
            @McpParam(name = "purpose", description = "e.g. PRIMARY_EMAIL, PRIMARY_PHONE, GENERAL_LOCATION, SHIPPING_LOCATION, BILLING_LOCATION", required = false) String purpose,
            @McpParam(name = "emailAddress", required = false) String emailAddress,
            @McpParam(name = "contactNumber", required = false) String contactNumber,
            @McpParam(name = "countryCode", required = false) String countryCode,
            @McpParam(name = "areaCode", required = false) String areaCode,
            @McpParam(name = "toName", required = false) String toName,
            @McpParam(name = "address1", required = false) String address1,
            @McpParam(name = "address2", required = false) String address2,
            @McpParam(name = "city", required = false) String city,
            @McpParam(name = "postalCode", required = false) String postalCode,
            @McpParam(name = "countryGeoId", required = false) String countryGeoId,
            @McpParam(name = "stateProvinceGeoId", required = false) String stateProvinceGeoId) throws McpToolException {
        String contactMechId;
        switch (type) {
            case "EMAIL_ADDRESS":
                if (UtilValidate.isEmpty(emailAddress)) throw new McpToolException("emailAddress is required");
                contactMechId = addEmail(ctx, partyId, emailAddress, purpose != null ? purpose : "PRIMARY_EMAIL");
                break;
            case "TELECOM_NUMBER":
                if (UtilValidate.isEmpty(contactNumber)) throw new McpToolException("contactNumber is required");
                contactMechId = addPhone(ctx, partyId, countryCode, areaCode, contactNumber, purpose != null ? purpose : "PRIMARY_PHONE");
                break;
            case "POSTAL_ADDRESS":
                if (UtilValidate.isEmpty(address1) || UtilValidate.isEmpty(city) || UtilValidate.isEmpty(countryGeoId)) {
                    throw new McpToolException("address1, city and countryGeoId are required");
                }
                contactMechId = addAddress(ctx, partyId, toName, address1, address2, city, postalCode, countryGeoId, stateProvinceGeoId,
                        purpose != null ? purpose : "GENERAL_LOCATION");
                break;
            default:
                throw new McpToolException("Unsupported type " + type);
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("partyId", partyId);
        out.put("contactMechId", contactMechId);
        return out;
    }

    private static String addEmail(McpCallContext ctx, String partyId, String emailAddress, String purpose) throws McpToolException {
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("partyId", partyId);
        p.put("emailAddress", emailAddress);
        p.put("contactMechPurposeTypeId", purpose);
        return (String) ctx.runService("createPartyEmailAddress", p).get("contactMechId");
    }

    private static String addPhone(McpCallContext ctx, String partyId, String countryCode, String areaCode, String contactNumber, String purpose) throws McpToolException {
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("partyId", partyId);
        if (countryCode != null) p.put("countryCode", countryCode);
        if (areaCode != null) p.put("areaCode", areaCode);
        p.put("contactNumber", contactNumber);
        p.put("contactMechPurposeTypeId", purpose);
        return (String) ctx.runService("createPartyTelecomNumber", p).get("contactMechId");
    }

    private static String addAddress(McpCallContext ctx, String partyId, String toName, String address1, String address2, String city,
                                     String postalCode, String countryGeoId, String stateProvinceGeoId, String purpose) throws McpToolException {
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("partyId", partyId);
        if (toName != null) p.put("toName", toName);
        p.put("address1", address1);
        if (address2 != null) p.put("address2", address2);
        p.put("city", city);
        if (postalCode != null) p.put("postalCode", postalCode);
        if (countryGeoId != null) p.put("countryGeoId", countryGeoId);
        if (stateProvinceGeoId != null) p.put("stateProvinceGeoId", stateProvinceGeoId);
        p.put("contactMechPurposeTypeId", purpose);
        return (String) ctx.runService("createPartyPostalAddress", p).get("contactMechId");
    }

    private static void addPurpose(McpCallContext ctx, String partyId, String contactMechId, String purpose) throws McpToolException {
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("partyId", partyId);
        p.put("contactMechId", contactMechId);
        p.put("contactMechPurposeTypeId", purpose);
        ctx.runService("createPartyContactMechPurpose", p);
    }

    @McpTool(topic = "supplier", name = "create", description = "Create a supplier: a party group with the SUPPLIER role.", readOnly = false, destructive = "false", order = 52)
    public static Object createSupplier(McpCallContext ctx,
            @McpParam(name = "groupName", required = true) String groupName,
            @McpParam(name = "email", required = false) String email,
            @McpParam(name = "countryCode", description = "Phone country code, e.g. 49", required = false) String countryCode,
            @McpParam(name = "contactNumber", description = "Phone number without country code", required = false) String contactNumber,
            @McpParam(name = "address", description = "toName, address1, address2, city, postalCode, stateProvinceGeoId, countryGeoId",
                    required = false, type = "object") Map<String, Object> address,
            @McpParam(name = "comments", required = false) String comments) throws McpToolException {
        return createSupplierParty(ctx, groupName, email, countryCode, contactNumber, address, comments);
    }

    @McpTool(topic = "supplier", name = "product_set", description = "Set a supplier product price and terms row.", readOnly = false, destructive = "false", order = 79)
    public static Object setSupplierProduct(McpCallContext ctx,
            @McpParam(name = "partyId", required = true) String partyId,
            @McpParam(name = "productId", required = true) String productId,
            @McpParam(name = "lastPrice", required = true) BigDecimal lastPrice,
            @McpParam(name = "currencyUomId", required = false) String currencyUomId,
            @McpParam(name = "minimumOrderQuantity", required = false) BigDecimal minimumOrderQuantity,
            @McpParam(name = "standardLeadTimeDays", required = false) Integer standardLeadTimeDays,
            @McpParam(name = "supplierProductId", description = "Supplier's own SKU", required = false) String supplierProductId,
            @McpParam(name = "availableFromDate", description = "yyyy-MM-dd or yyyy-MM-dd HH:mm:ss; defaults to now",
                    required = false) String availableFromDate,
            @McpParam(name = "quantityUomId", required = false) String quantityUomId,
            @McpParam(name = "comments", required = false) String comments) throws McpToolException {
        return upsertSupplierProduct(ctx, partyId, productId, lastPrice, currencyUomId, minimumOrderQuantity,
                standardLeadTimeDays, supplierProductId, parseTimestamp(availableFromDate), quantityUomId, comments);
    }

    @McpTool(topic = "supplier", name = "import", description = "Import supplier and supplier-product rows from a list.",
            readOnly = false, destructive = "false", order = 65)
    public static Object importSuppliers(McpCallContext ctx,
            @McpParam(name = "rows", required = true, type = "array") List<Object> rows,
            @McpParam(name = "apply", required = false) Boolean apply,
            @McpParam(name = "force", required = false) Boolean force) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        ImportDiff diff = new ImportDiff(apply, force);
        List<Map<String, Object>> list = ImportDiff.rows(rows);
        for (int i = 0; i < list.size(); i++) {
            Map<String, Object> row = list.get(i);
            String partyId = ImportDiff.str(row, "partyId");
            String supplierName = ImportDiff.str(row, "supplier", "supplierName");
            String email = ImportDiff.str(row, "email");
            String phone = ImportDiff.str(row, "phone");
            String countryGeoId = ImportDiff.str(row, "country", "countryGeoId");
            String sku = ImportDiff.str(row, "sku", "productId");
            BigDecimal price = ImportDiff.decimal(row, "price", "lastPrice");
            String currency = ImportDiff.str(row, "currency");
            Integer leadDays = ImportDiff.integer(row, "leadDays", "standardLeadTimeDays");
            BigDecimal minQty = ImportDiff.decimal(row, "minQty", "minimumOrderQuantity");
            String supplierSku = ImportDiff.str(row, "supplierSku", "supplierProductId");

            String resolvedPartyId = partyId;
            boolean newSupplier = false;
            try {
                if (UtilValidate.isEmpty(resolvedPartyId)) {
                    if (UtilValidate.isEmpty(supplierName)) {
                        diff.doubt(i, "row has no supplier, supplierName or partyId");
                        continue;
                    }
                    List<GenericValue> matches = EntityQuery.use(delegator).from("PartyGroup")
                            .where("groupName", supplierName).queryList();
                    if (matches.size() > 1) {
                        diff.doubt(i, "multiple PartyGroup rows match groupName " + supplierName);
                        continue;
                    } else if (matches.size() == 1) {
                        resolvedPartyId = matches.get(0).getString("partyId");
                    } else {
                        newSupplier = true;
                    }
                }
            } catch (GenericEntityException e) {
                throw new McpToolException("Supplier lookup failed: " + e.getMessage());
            }

            if (UtilValidate.isNotEmpty(sku)) {
                try {
                    if (EntityQuery.use(delegator).from("Product").where("productId", sku).queryOne() == null) {
                        diff.doubt(i, "product not found: " + sku);
                        continue;
                    }
                } catch (GenericEntityException e) {
                    throw new McpToolException("Product lookup failed: " + e.getMessage());
                }
            }

            String resolvedCurrency = UtilValidate.isNotEmpty(currency) ? currency
                    : (UtilValidate.isNotEmpty(ctx.getCurrencyUomId()) ? ctx.getCurrencyUomId() : "USD");
            BigDecimal resolvedMinQty = minQty != null ? minQty : BigDecimal.ONE;

            if (newSupplier) {
                Map<String, Object> data = new LinkedHashMap<>();
                data.put("groupName", supplierName);
                if (email != null) data.put("email", email);
                if (phone != null) data.put("phone", phone);
                if (countryGeoId != null) data.put("countryGeoId", countryGeoId);
                diff.create("supplier", supplierName, data);
            }

            GenericValue existingSp = (!newSupplier && UtilValidate.isNotEmpty(sku))
                    ? findActiveSupplierProduct(delegator, resolvedPartyId, sku, resolvedCurrency, resolvedMinQty, UtilDateTime.nowTimestamp())
                    : null;
            if (UtilValidate.isNotEmpty(sku) && price != null) {
                Map<String, Object> spData = new LinkedHashMap<>();
                spData.put("productId", sku);
                spData.put("lastPrice", price);
                if (existingSp != null) {
                    Map<String, Object> before = new LinkedHashMap<>();
                    before.put("lastPrice", existingSp.getBigDecimal("lastPrice"));
                    diff.update("supplierProduct", sku, before, spData);
                } else {
                    diff.create("supplierProduct", sku, spData);
                }
            } else if (!newSupplier) {
                diff.unchanged("supplier", resolvedPartyId);
            }

            if (diff.canWrite()) {
                if (newSupplier) {
                    Map<String, Object> addr = null;
                    if (UtilValidate.isNotEmpty(countryGeoId)) {
                        addr = new LinkedHashMap<>();
                        addr.put("countryGeoId", countryGeoId);
                    }
                    Map<String, Object> created = createSupplierParty(ctx, supplierName, email, null, phone, addr, null);
                    resolvedPartyId = (String) created.get("partyId");
                    diff.written("supplier", supplierName, created);
                }
                if (UtilValidate.isNotEmpty(sku) && price != null) {
                    Map<String, Object> spResult = upsertSupplierProduct(ctx, resolvedPartyId, sku, price, resolvedCurrency,
                            resolvedMinQty, leadDays, supplierSku, null, null, null);
                    diff.written("supplierProduct", sku, spResult);
                }
            }
        }
        return diff.result();
    }

    private static Map<String, Object> createSupplierParty(McpCallContext ctx, String groupName, String email,
            String countryCode, String contactNumber, Map<String, Object> address, String comments) throws McpToolException {
        Map<String, Object> group = new LinkedHashMap<>();
        group.put("groupName", groupName);
        if (comments != null) group.put("comments", comments);
        String partyId = (String) ctx.runService("createPartyGroup", group).get("partyId");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("partyId", partyId);
        out.put("groupName", groupName);
        Map<String, Object> role = new LinkedHashMap<>();
        role.put("partyId", partyId);
        role.put("roleTypeId", "SUPPLIER");
        ctx.runService("createPartyRole", role);
        if (UtilValidate.isNotEmpty(email)) {
            out.put("emailContactMechId", addEmail(ctx, partyId, email, "PRIMARY_EMAIL"));
        }
        if (UtilValidate.isNotEmpty(contactNumber)) {
            out.put("phoneContactMechId", addPhone(ctx, partyId, countryCode, null, contactNumber, "PRIMARY_PHONE"));
        }
        if (address != null && UtilValidate.isNotEmpty((String) address.get("address1"))) {
            String cmId = addAddress(ctx, partyId,
                    (String) address.get("toName"),
                    (String) address.get("address1"),
                    (String) address.get("address2"),
                    (String) address.get("city"),
                    (String) address.get("postalCode"),
                    (String) address.get("countryGeoId"),
                    (String) address.get("stateProvinceGeoId"),
                    "GENERAL_LOCATION");
            out.put("postalAddressContactMechId", cmId);
        }
        return out;
    }

    private static Map<String, Object> upsertSupplierProduct(McpCallContext ctx, String partyId, String productId,
            BigDecimal lastPrice, String currencyUomId, BigDecimal minimumOrderQuantity, Integer standardLeadTimeDays,
            String supplierProductId, Timestamp availableFromDate, String quantityUomId, String comments) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        String curr = UtilValidate.isNotEmpty(currencyUomId) ? currencyUomId
                : (UtilValidate.isNotEmpty(ctx.getCurrencyUomId()) ? ctx.getCurrencyUomId() : "USD");
        BigDecimal moq = minimumOrderQuantity != null ? minimumOrderQuantity : BigDecimal.ONE;
        Timestamp now = UtilDateTime.nowTimestamp();
        Timestamp fromDate = availableFromDate != null ? availableFromDate : now;
        GenericValue existing = findActiveSupplierProduct(delegator, partyId, productId, curr, moq, now);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("partyId", partyId);
        out.put("productId", productId);
        out.put("currencyUomId", curr);
        out.put("minimumOrderQuantity", moq);
        out.put("lastPrice", lastPrice);
        if (existing != null) {
            Map<String, Object> p = new LinkedHashMap<>();
            p.put("productId", existing.getString("productId"));
            p.put("partyId", existing.getString("partyId"));
            p.put("availableFromDate", existing.getTimestamp("availableFromDate"));
            p.put("currencyUomId", existing.getString("currencyUomId"));
            p.put("minimumOrderQuantity", existing.getBigDecimal("minimumOrderQuantity"));
            p.put("lastPrice", lastPrice);
            if (standardLeadTimeDays != null) p.put("standardLeadTimeDays", BigDecimal.valueOf(standardLeadTimeDays.longValue()));
            if (UtilValidate.isNotEmpty(supplierProductId)) p.put("supplierProductId", supplierProductId);
            if (UtilValidate.isNotEmpty(quantityUomId)) p.put("quantityUomId", quantityUomId);
            if (comments != null) p.put("comments", comments);
            ctx.runService("updateSupplierProduct", p);
            out.put("action", "updated");
            out.put("availableFromDate", existing.getTimestamp("availableFromDate"));
        } else {
            Map<String, Object> p = new LinkedHashMap<>();
            p.put("productId", productId);
            p.put("partyId", partyId);
            p.put("availableFromDate", fromDate);
            p.put("currencyUomId", curr);
            p.put("minimumOrderQuantity", moq);
            p.put("lastPrice", lastPrice);
            p.put("supplierProductId", UtilValidate.isNotEmpty(supplierProductId) ? supplierProductId : productId);
            if (standardLeadTimeDays != null) p.put("standardLeadTimeDays", BigDecimal.valueOf(standardLeadTimeDays.longValue()));
            if (UtilValidate.isNotEmpty(quantityUomId)) p.put("quantityUomId", quantityUomId);
            if (comments != null) p.put("comments", comments);
            ctx.runService("createSupplierProduct", p);
            out.put("action", "created");
            out.put("availableFromDate", fromDate);
        }
        return out;
    }

    private static GenericValue findActiveSupplierProduct(Delegator delegator, String partyId, String productId,
            String currencyUomId, BigDecimal minimumOrderQuantity, Timestamp now) throws McpToolException {
        try {
            List<EntityCondition> cond = new ArrayList<>();
            cond.add(EntityCondition.makeCondition("productId", productId));
            cond.add(EntityCondition.makeCondition("partyId", partyId));
            cond.add(EntityCondition.makeCondition("currencyUomId", currencyUomId));
            cond.add(EntityCondition.makeCondition("minimumOrderQuantity", minimumOrderQuantity));
            cond.add(EntityCondition.makeCondition("availableFromDate", EntityOperator.LESS_THAN_EQUAL_TO, now));
            cond.add(EntityCondition.makeCondition(
                    EntityCondition.makeCondition("availableThruDate", EntityOperator.EQUALS, null),
                    EntityOperator.OR,
                    EntityCondition.makeCondition("availableThruDate", EntityOperator.GREATER_THAN, now)));
            List<GenericValue> rows = EntityQuery.use(delegator).from("SupplierProduct").where(cond).queryList();
            return rows.isEmpty() ? null : rows.get(0);
        } catch (GenericEntityException e) {
            throw new McpToolException("SupplierProduct lookup failed: " + e.getMessage());
        }
    }

    private static Timestamp parseTimestamp(String s) throws McpToolException {
        if (UtilValidate.isEmpty(s)) return null;
        try {
            String v = s.trim();
            if (v.length() == 10) v = v + " 00:00:00";
            return Timestamp.valueOf(v);
        } catch (IllegalArgumentException e) {
            throw new McpToolException("Invalid date/time: " + s);
        }
    }

    @McpResource(uri = "scipio://party/{partyId}", name = "Party", description = "One party as JSON: identity, roles, contact mechanisms.",
            mimeType = "application/json")
    public static String partyResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        return JsonRpc.writePretty(getParty(ctx, uriParams.get("partyId")));
    }
}
