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
package com.ilscipio.scipio.commerceprofile.test;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * W0-04 smoke test: the commerce profile (no manufacturing, no humanres) takes a sales order through approval to an
 * invoice. Every step is strict: an error result fails the test. Unlike the older order tests, it does not skip on
 * an error result.
 *
 * <p>Run: {@code gradlew.bat runTest -PtestComponent=commerce-profile -PtestCase=order-to-invoice-smoke-test}</p>
 */
public class OrderToInvoiceSmokeTest extends OFBizTestCase {

    private GenericValue userLogin;

    public OrderToInvoiceSmokeTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
        assertNotNull("UserLogin system (seed data)", userLogin);
    }

    /** The profile must not load the two dropped components, and must load the kept ones. */
    public void testProfileComponents() throws Exception {
        assertFalse("manufacturing is loaded", org.ofbiz.base.component.ComponentConfig.isComponentEnabled("manufacturing"));
        assertFalse("humanres is loaded", org.ofbiz.base.component.ComponentConfig.isComponentEnabled("humanres"));
        for (String c : new String[] {"party", "product", "order", "accounting", "cms", "shop", "workeffort", "marketing", "commerce-profile"}) {
            assertTrue(c + " is not loaded", org.ofbiz.base.component.ComponentConfig.isComponentEnabled(c));
        }
        // The entity models of the two components are gone with them; the kept models stay.
        java.util.Set<String> entityNames = delegator.getModelReader().getEntityNames();
        assertFalse("manufacturing entity model is loaded", entityNames.contains("ProductManufacturingRule"));
        assertFalse("humanres entity model is loaded", entityNames.contains("EmplPosition"));
        // The stubs of commerce-profile/pod-model stand in for the entities that kept components refer to.
        assertTrue(entityNames.contains("TechDataCalendar") && entityNames.contains("EmplPositionType"));
        assertTrue(entityNames.contains("WorkEffort") && entityNames.contains("MarketingCampaign") && entityNames.contains("OrderHeader"));
    }

    static Map<String, Object> ok(String step, Map<String, Object> result) {
        if (ServiceUtil.isError(result) || ServiceUtil.isFailure(result)) {
            fail(step + " failed: " + ServiceUtil.getErrorMessage(result));
        }
        return result;
    }

    private static String createParty(org.ofbiz.service.LocalDispatcher dispatcher, GenericValue userLogin, String kind, String partyId, String roleTypeId) throws Exception {
        Map<String, Object> in = new java.util.HashMap<>();
        in.put("partyId", partyId);
        in.put("userLogin", userLogin);
        if ("person".equals(kind)) {
            in.put("firstName", "Smoke");
            in.put("lastName", "Customer");
            ok("createPerson", dispatcher.runSync("createPerson", in));
        } else {
            in.put("groupName", "Smoke Store Company");
            ok("createPartyGroup", dispatcher.runSync("createPartyGroup", in));
        }
        if (roleTypeId != null) {
            ok("createPartyRole " + roleTypeId, dispatcher.runSync("createPartyRole",
                    UtilMisc.<String, Object>toMap("partyId", partyId, "roleTypeId", roleTypeId, "userLogin", userLogin)));
        }
        return partyId;
    }

    /**
     * The test creates its own data: the shop demo data does not load in the pod profile (DemoCatalogData.xml needs the
     * manufacturing demo data), and a pod holds seed data only.
     */
    public static String createSalesOrder(org.ofbiz.service.LocalDispatcher dispatcher, org.ofbiz.entity.Delegator delegator, GenericValue userLogin) throws Exception {
        String suffix = Long.toString(System.currentTimeMillis() % 1000000);
        // The order ECAs write a system note for the party "admin" (order Eecas.java, Secas.java): the party must exist in the store.
        if (EntityQuery.use(delegator).from("Party").where("partyId", "admin").queryOne() == null) {
            createParty(dispatcher, userLogin, "person", "admin", null);
        }
        String customer = createParty(dispatcher, userLogin, "person", "W004C" + suffix, "CUSTOMER");
        String company = createParty(dispatcher, userLogin, "group", "W004O" + suffix, "BILL_FROM_VENDOR");
        String productId = "W004P" + suffix;
        ok("createProduct", dispatcher.runSync("createProduct", UtilMisc.<String, Object>toMap("productId", productId,
                "productTypeId", "FINISHED_GOOD", "internalName", "Smoke product", "userLogin", userLogin)));

        String storeId = "W004S" + suffix;
        delegator.create("ProductStore", UtilMisc.toMap("productStoreId", storeId, "storeName", "Smoke store", "payToPartyId", company,
                "defaultCurrencyUomId", "USD", "defaultLocaleString", "en", "prorateShipping", "N", "prorateTaxes", "N",
                "requireInventory", "N", "checkGcBalance", "N", "explodeOrderItems", "N", "manualAuthIsCapture", "N", "reserveInventory", "N"));

        // 1. Create the order.
        Map<String, Object> ctx = UtilMisc.<String, Object>toMap("partyId", customer, "orderTypeId", "SALES_ORDER", "currencyUom", "USD",
                "productStoreId", storeId);

        List<GenericValue> orderItems = new LinkedList<>();
        GenericValue orderItem = delegator.makeValue("OrderItem", UtilMisc.toMap("orderItemSeqId", "00001", "orderItemTypeId", "PRODUCT_ORDER_ITEM",
                "productId", productId, "quantity", new BigDecimal("2"), "selectedAmount", BigDecimal.ZERO));
        orderItem.set("isPromo", "N");
        orderItem.set("isModifiedPrice", "N");
        orderItem.set("unitPrice", new BigDecimal("10.00"));
        orderItem.set("unitListPrice", new BigDecimal("12.00"));
        orderItem.set("statusId", "ITEM_CREATED");
        orderItems.add(orderItem);
        ctx.put("orderItems", orderItems);
        ctx.put("orderTerms", new LinkedList<GenericValue>());
        ctx.put("orderAdjustments", new LinkedList<GenericValue>());

        ctx.put("placingCustomerPartyId", customer);
        ctx.put("endUserCustomerPartyId", customer);
        ctx.put("shipToCustomerPartyId", customer);
        ctx.put("billToCustomerPartyId", customer);
        ctx.put("billFromVendorPartyId", company);
        ctx.put("userLogin", userLogin);

        Map<String, Object> stored = ok("storeOrder", dispatcher.runSync("storeOrder", ctx));
        String orderId = (String) stored.get("orderId");
        assertNotNull("orderId", orderId);
        GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
        assertNotNull("OrderHeader " + orderId, header);
        assertEquals("SALES_ORDER", header.getString("orderTypeId"));
        assertEquals(new BigDecimal("20.00"), header.getBigDecimal("grandTotal").setScale(2));
        return orderId;
    }

    public void testSalesOrderToInvoice() throws Exception {
        String orderId = createSalesOrder(dispatcher, delegator, userLogin);
        GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();

        // 2. Approve the order (this approves the items).
        ok("changeOrderStatus", dispatcher.runSync("changeOrderStatus", UtilMisc.<String, Object>toMap("orderId", orderId,
                "statusId", "ORDER_APPROVED", "userLogin", userLogin)));
        header.refresh();
        assertEquals("ORDER_APPROVED", header.getString("statusId"));

        // 3. Invoice the order.
        Map<String, Object> invoiced = ok("createInvoiceForOrderAllItems", dispatcher.runSync("createInvoiceForOrderAllItems",
                UtilMisc.<String, Object>toMap("orderId", orderId, "userLogin", userLogin)));
        String invoiceId = (String) invoiced.get("invoiceId");
        assertNotNull("invoiceId", invoiceId);
        GenericValue invoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
        assertNotNull("Invoice " + invoiceId, invoice);
        assertEquals("SALES_INVOICE", invoice.getString("invoiceTypeId"));
        assertTrue("OrderItemBilling links the order to the invoice",
                EntityQuery.use(delegator).from("OrderItemBilling").where("orderId", orderId, "invoiceId", invoiceId).queryCount() > 0);

        // 4. The invoice total is the order total.
        BigDecimal invoiceTotal = BigDecimal.ZERO;
        for (GenericValue item : EntityQuery.use(delegator).from("InvoiceItem").where("invoiceId", invoiceId).queryList()) {
            BigDecimal quantity = item.getBigDecimal("quantity") != null ? item.getBigDecimal("quantity") : BigDecimal.ONE;
            invoiceTotal = invoiceTotal.add(item.getBigDecimal("amount").multiply(quantity));
        }
        assertEquals(header.getBigDecimal("grandTotal").setScale(2), invoiceTotal.setScale(2));
    }
}
