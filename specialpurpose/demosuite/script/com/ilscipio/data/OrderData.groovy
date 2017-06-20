import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

public class OrderData extends DataGeneratorGroovyBaseScript {

    OrderData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - ORDER DATA-=-=-=-", "");
    }

    final List orderTypes = [
        "PURCHASE_ORDER",
        "SALES_ORDER"
    ]
    final List orderStatusTypes = [
        "ORDER_CREATED",
        "ORDER_COMPLETED"
    ];

    public void init() {
        productCount = from("Product").queryCount();
        totalProductCount = (productCount < Integer.MAX_VALUE) ? (int) productCount : Integer.MAX_VALUE - 1;
        context.totalProductCount = totalProductCount;
    }

    List prepareData(int index) throws Exception {
        List toBeStored = new ArrayList();
        List orderItems = new ArrayList();
        // Create OrderHeader
        String orderId = "GEN_" + delegator.getNextSeqId("demo-orderheader");
        // Create OrderItem (between 1 and 3)
        int orderItemCount = UtilRandom.getRandomInt(1,3);
        BigDecimal remainingSubTotal = new BigDecimal(0.00);
        BigDecimal grandTotal = new BigDecimal(0.00);
        
        String prodCatalogId = context.prodCatalogId ?: "DemoCatalog";
        String productStoreId = context.productStoreId ?: null;
        if (!productStoreId) {
            List productStoreCatalogs = EntityUtil.filterByDate(from("ProductStoreCatalog").where("prodCatalogId", prodCatalogId).queryList());
            if (productStoreCatalogs) {
                productStoreId = productStoreCatalogs[0].productStoreId;
            }
        }
        
        for (int orderItemSeqId = 1; orderItemSeqId <= orderItemCount; orderItemSeqId++) {
            EntityFindOptions efo = new EntityFindOptions();
            efo.setMaxRows(1);
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalProductCount));
            products = from("Product").query(efo);
            if (products) {
                product = products.get(0);
                productPrices = product.getRelated("ProductPrice", null, null, false);
                defaultPrice = 0;
                listPrice = 0;
                for (productPrice in productPrices) {
                    if (productPrice.productPriceTypeId.equals("DEFAULT_PRICE"))
                        defaultPrice = productPrice.price;
                    else (productPrice.productPriceTypeId.equals("LIST_PRICE"))
                    listPrice = productPrice.price;
                }

                String productId = product.productId;
                BigDecimal quantity = new BigDecimal(UtilRandom.getRandomInt(0,10));
                BigDecimal unitPrice= new BigDecimal(defaultPrice);
                BigDecimal unitListPrice= new BigDecimal(listPrice);
                BigDecimal selectedAmount = new BigDecimal(0.0);
                BigDecimal itemCost = BigDecimal.ZERO;
                itemCost = unitPrice.multiply(new BigDecimal(quantity));
                remainingSubTotal =  remainingSubTotal.add(itemCost);
                grandTotal = grandTotal.add(itemCost);

                fields = UtilMisc.toMap("orderId", orderId,"orderItemSeqId","0000"+orderItemSeqId,"orderItemTypeId","PRODUCT_ORDER_ITEM","productId",
                        productId,"prodCatalogId",prodCatalogId,"isPromo","N","quantity",quantity,"selectedAmount",selectedAmount,
                        "unitPrice",unitPrice,"unitListPrice",unitListPrice,"isModifiedPrice","N","itemDescription","Round Gizmo",
                        "correspondingPoId","","statusId","ITEM_APPROVED");

                GenericValue orderItem = delegator.makeValue("OrderItem", fields);
                orderItems.add(orderItem);
            }
        }

        String orderTypeId = orderTypes.get(UtilRandom.random(orderTypes));
        String orderName="Demo Order";

        orderSalesChannelList = from("Enumeration").where(["enumTypeId" : "ORDER_SALES_CHANNEL"]).queryList();
        orderSalesChannel = orderSalesChannelList.get(UtilRandom.random(orderSalesChannelList));

        Timestamp orderDate = UtilRandom.generateRandomTimestamp(context);
        String statusId = orderStatusTypes.get(UtilRandom.random(orderStatusTypes));
        Map fields = UtilMisc.toMap("orderId", orderId,"orderTypeId",orderTypeId,"orderName",orderName,"salesChannelEnumId",
                orderSalesChannel.enumId,"orderDate",orderDate,"priority","2","entryDate",orderDate,"statusId",statusId,
                "currencyUom","USD","webSiteId","OrderEntry","remainingSubTotal",remainingSubTotal,"grandTotal",grandTotal,
                "productStoreId", productStoreId);

        GenericValue orderHeader = delegator.makeValue("OrderHeader", fields);
        toBeStored.add(orderHeader);
        toBeStored.addAll(orderItems);

        // Create orderRole
        fields = UtilMisc.toMap("orderId", orderId,"partyId","Company","roleTypeId","BILL_FROM_VENDOR");
        GenericValue orderRole1 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole1);

        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","BILL_TO_CUSTOMER");
        GenericValue orderRole2 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole2);

        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","END_USER_CUSTOMER");
        GenericValue orderRole3 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole3);

        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","PLACING_CUSTOMER");
        GenericValue orderRole4 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole4);

        fields = UtilMisc.toMap("orderId", orderId,"partyId","DemoCustomer","roleTypeId","SHIP_TO_CUSTOMER");
        GenericValue orderRole5 = delegator.makeValue("OrderRole", fields);
        toBeStored.add(orderRole5);

        // Create OrderStatus
        String orderStatusId = "GEN_"+delegator.getNextSeqId("demo-orderstatusid");
        fields = UtilMisc.toMap("orderId", orderId,"orderStatusId",orderStatusId,"statusId","ORDER_CREATED",
                "statusDatetime",orderDate,"statusUserLogin","admin");
        GenericValue orderStatus = delegator.makeValue("OrderStatus", fields);
        toBeStored.add(orderStatus);

        if (UtilRandom.getRandomBoolean()==true) {
            orderStatusId = "GEN_"+delegator.getNextSeqId("demo-orderstatusid");
            fields = UtilMisc.toMap("orderId", orderId,"orderStatusId",orderStatusId,"statusId","ORDER_COMPLETED",
                    "statusDatetime",orderDate,"statusUserLogin","admin");
            GenericValue orderStatus2 = delegator.makeValue("OrderStatus", fields);
            toBeStored.add(orderStatus2);
        }

        return toBeStored;
    }
}