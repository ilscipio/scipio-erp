package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderItem;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderRole;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderStatus;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.LocalDemoDataHelper;

/**
 * Demo data generator that doesn't rely in any external data provider
 * 
 * @author jsoto
 *
 */
public class LocalDataGenerator extends DataGenerator {

    private final static String LOCAL_DATA_GENERATOR = "local";

    private final LocalDemoDataHelper helper;

    public LocalDataGenerator(DemoDataHelper helper) {
        super(helper);
        this.helper = (LocalDemoDataHelper) helper;
    }

    final List<String> orderTypes = UtilMisc.toList("PURCHASE_ORDER", "SALES_ORDER");
    final List<String> orderStatusTypes = UtilMisc.toList("ORDER_CREATED", "ORDER_COMPLETED");

    @Override
    public List<? extends DemoDataObject> retrieveData() throws Exception {
        Delegator delegator = helper.getDelegator();
        Map<String, Object> context = helper.getContext();

        List<DemoDataOrder> orders = new ArrayList<DemoDataOrder>();

        for (int i = 0; i < helper.getCount(); i++) {
            DemoDataOrder order = new DemoDataOrder();
            BigDecimal remainingSubTotal = new BigDecimal(0.00);
            BigDecimal grandTotal = new BigDecimal(0.00);

            // Create OrderHeader
            order.setOrderId("GEN_" + delegator.getNextSeqId("demo-orderheader"));

            // Create OrderItem (between 1 and 3)
            int orderItemCount = UtilRandom.getRandomInt(1, 3);

            EntityFindOptions efo = new EntityFindOptions();
            efo.setMaxRows(1);
            for (int orderItemSeqId = 1; orderItemSeqId <= orderItemCount; orderItemSeqId++) {
                efo.setOffset(UtilRandom.getRandomInt(0, (int) context.get("totalProductCount") - 1));
                List<GenericValue> products = delegator.findList("Product", null, null, null, efo, true);
                if (UtilValidate.isNotEmpty(products)) {
                    DemoDataOrderItem item = order.new DemoDataOrderItem();

                    GenericValue product = products.get(0);
                    List<GenericValue> productPrices = product.getRelated("ProductPrice", null, null, false);
                    BigDecimal defaultPrice = BigDecimal.ZERO;
                    BigDecimal listPrice = BigDecimal.ZERO;
                    for (GenericValue productPrice : productPrices) {
                        if (productPrice.get("productPriceTypeId").equals("DEFAULT_PRICE")) {
                            defaultPrice = productPrice.getBigDecimal("price");
                        } else if (productPrice.get("productPriceTypeId").equals("LIST_PRICE")) {
                            listPrice = productPrice.getBigDecimal("price");
                        }
                    }

                    item.setOrderItemId(String.valueOf(orderItemSeqId));
                    item.setProductId(product.getString("productId"));
                    item.setQuantity(new BigDecimal(UtilRandom.getRandomInt(0, 10)));

                    item.setSelectedAmount(new BigDecimal(0.0));
                    item.setUnitPrice(defaultPrice);
                    item.setUnitListPrice(listPrice);
                    item.setItemCost(item.getUnitPrice().multiply(item.getQuantity()));

                    remainingSubTotal = remainingSubTotal.add(item.getItemCost());
                    grandTotal = grandTotal.add(item.getItemCost());

                    order.addOrderItem(item);
                }
            }

            order.setOrderDate(UtilRandom.generateRandomTimestamp(context));
            order.setOrderType(orderTypes.get(UtilRandom.random(orderTypes)));
            order.setOrderName("Demo Order " + UtilDateTime.timeStampToString(order.getOrderDate(), (TimeZone) context.get("timeZone"), (Locale) context.get("locale")));
            order.setRemainingSubTotal(remainingSubTotal);
            order.setGrandTotal(grandTotal);

            if (order.getOrderType().equals("SALES_ORDER")) {
                List<GenericValue> orderSalesChannelList = EntityQuery.use(delegator).from("Enumeration").where(UtilMisc.toMap("enumTypeId", "ORDER_SALES_CHANNEL")).queryList();
                GenericValue orderSalesChannel = orderSalesChannelList.get(UtilRandom.random(orderSalesChannelList));
                order.setChannel(orderSalesChannel.getString("enumId"));
            }

            // Create basic roles
            List<DemoDataOrderRole> roles = order.getOrderRoles();
            roles.add(order.new DemoDataOrderRole("INTERNAL_ORGANIZATIO", (String) context.get("partyGroupId")));
            roles.add(order.new DemoDataOrderRole("CUSTOMER", (String) context.get("partyCustomerId")));

            // Create advanced roles, if they exist (optional)
            GenericValue partyRoleEndUserCustomer = EntityQuery.use(delegator).from("PartyRole").where("partyId", context.get("partyCustomerId"), "roleTypeId", "END_USER_CUSTOMER")
                    .cache(true).queryOne();
            if (UtilValidate.isNotEmpty(partyRoleEndUserCustomer)) {
                roles.add(order.new DemoDataOrderRole((String) context.get("partyCustomerId"), "END_USER_CUSTOMER"));
            }
            GenericValue partyRolePlacingCustomer = EntityQuery.use(delegator).from("PartyRole").where("partyId", context.get("partyCustomerId"), "roleTypeId", "PLACING_CUSTOMER")
                    .cache(true).queryOne();
            if (UtilValidate.isNotEmpty(partyRolePlacingCustomer)) {
                roles.add(order.new DemoDataOrderRole((String) context.get("partyCustomerId"), "PLACING_CUSTOMER"));
            }
            GenericValue partyRoleShipToCustomer = EntityQuery.use(delegator).from("PartyRole").where("partyId", context.get("partyCustomerId"), "roleTypeId", "SHIP_TO_CUSTOMER")
                    .cache(true).queryOne();
            if (UtilValidate.isNotEmpty(partyRoleShipToCustomer)) {
                roles.add(order.new DemoDataOrderRole((String) context.get("partyCustomerId"), "SHIP_TO_CUSTOMER"));
            }

            // Create OrderStatus
            String orderStatusId = "GEN_" + delegator.getNextSeqId("demo-orderstatusid");
            List<DemoDataOrderStatus> statuses = order.getOrderStatuses();
            statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_CREATED", order.getOrderDate()));
            if (UtilRandom.getRandomBoolean() == true) {
                orderStatusId = "GEN_" + delegator.getNextSeqId("demo-orderstatusid");
                if (UtilRandom.getRandomBoolean() == true) {
                    statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_COMPLETED", order.getOrderDate()));
                } else {
                    statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_CANCELLED", order.getOrderDate()));
                }
            }

            orders.add(order);
        }

        return orders;
    }

    @Override
    public DemoDataObject handleData(Object result, String format) {
        return null;
    }

    @Override
    public String getDataGeneratorName() {
        return LOCAL_DATA_GENERATOR;
    }

}
