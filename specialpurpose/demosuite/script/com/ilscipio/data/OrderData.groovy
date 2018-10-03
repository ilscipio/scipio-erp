import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.product.catalog.CatalogWorker
import org.ofbiz.product.store.ProductStoreWorker

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper.dataTypeEnum
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
    
    public String getDataType() {
        return dataTypeEnum.ORDER;
    }

    public void init() {
        productCount = from("Product").queryCount();
        if (productCount == 0) {
            throw new Exception("This service depends on product data to be present. Please load product data or generate product demo data first and try again.");
        }
        totalProductCount = (productCount < Integer.MAX_VALUE) ? (int) productCount : Integer.MAX_VALUE - 1;
        
        totalPartyGroupCount = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").queryCount();
        if (totalPartyGroupCount == 0) {
            throw new Exception("This service depends on party group data to be present. Please load party group data or generate party group demo data first and try again.");
        }
        
        totalPartyCustomerCount = from("PartyRole").where("roleTypeId", "CUSTOMER").queryCount();
        if (totalPartyCustomerCount == 0) {
            throw new Exception("This service depends on party customer data to be present. Please load party customer data or generate party customer demo data first and try again.");
        }
        context.totalProductCount = totalProductCount;
        context.totalPartyGroupCount = totalPartyCustomerCount;
        context.totalPartyCustomerCount = totalPartyCustomerCount;
    }

    List prepareData(int index, DemoDataObject orderData) throws Exception {
        List toBeStored = new ArrayList();
        List orderItems = new ArrayList();
        
        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);
        
        DataGenerator generator = context.generator;
        
        // Create OrderHeader
        String orderId = "GEN_" + delegator.getNextSeqId("demo-orderheader");
        // Create OrderItem (between 1 and 3)
        int orderItemCount = UtilRandom.getRandomInt(1,3);
        BigDecimal remainingSubTotal = new BigDecimal(0.00);
        BigDecimal grandTotal = new BigDecimal(0.00);
        
        String prodCatalogId = context.prodCatalogId ?: null;
        String productStoreId = context.productStoreId ?: null;
        String partyGroupId = context.partyGroupId ?: null;
        String partyCustomerId = context.partyCustomerId ?: null;
        
        // Check if we got a valid ProductStore
        if (!productStoreId) {
            List productStoreCatalogs = EntityUtil.filterByDate(from("ProductStoreCatalog").where("prodCatalogId", prodCatalogId).queryList());
            if (productStoreCatalogs) {
                productStoreId = productStoreCatalogs[0].productStoreId;
            }
        } else {
            if (!ProductStoreWorker.getProductStore(productStoreId))
               productStoreId = null;
        }        
        if (!productStoreId) {
            throw new Exception("Product store not found or invalid.");
        }        
        GenericValue webSite = from("WebSite").where("productStoreId", productStoreId).queryFirst();
        if (!webSite) {
            throw new Exception("Website not found or invalid.");
        }
        
        // If no partyGroupId is passed, pick one randomly
        if (!partyGroupId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyGroupCount));
            partyGroup = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").query(efo);
            if (partyGroup) {
                partyGroupId = partyGroup.get("partyId");
            }
        }    
        if (!partyGroupId)
            throw new Exception("Party group not found or invalid.");
            
        // If no partyCustomerId is passed, pick one randomly
        if (!partyCustomerId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyCustomerCount));
            partyCustomer = from("PartyRole").where("roleTypeId", "PLACING_CUSTOMER").query(efo);
            if (partyCustomer) {
                partyCustomerId = partyCustomer.get("partyId");
            }
        }
        if (!partyCustomerId)
            throw new Exception("Party customer not found or invalid.");
       
        for (int orderItemSeqId = 1; orderItemSeqId <= orderItemCount; orderItemSeqId++) {
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
                "currencyUom","USD","webSiteId",webSite.getString("webSiteId"),"remainingSubTotal",remainingSubTotal,"grandTotal",grandTotal,
                "productStoreId", productStoreId);

        GenericValue orderHeader = delegator.makeValue("OrderHeader", fields);
        toBeStored.add(orderHeader);
        toBeStored.addAll(orderItems);

        if (orderHeader && orderItems) {
            // Create basic orderRoles, basically the ones we know for sure they exist
            fields = UtilMisc.toMap("orderId", orderId,"partyId", partyGroupId, "roleTypeId", "INTERNAL_ORGANIZATIO");
            GenericValue orderRole1 = delegator.makeValue("OrderRole", fields);
            toBeStored.add(orderRole1);
            fields = UtilMisc.toMap("orderId", orderId,"partyId", partyCustomerId,"roleTypeId","CUSTOMER");
            GenericValue orderRole2 = delegator.makeValue("OrderRole", fields);
            toBeStored.add(orderRole2);
    
            // Create advanced roles, if they exist (optional)
            partyRoleEndUserCustomer = from("PartyRole").where("partyId", partyCustomerId, "roleTypeId", "END_USER_CUSTOMER").cache(true).queryOne();
            if (!partyRoleEndUserCustomer) {
                fields = UtilMisc.toMap("orderId", orderId,"partyId", partyCustomerId, "roleTypeId", "END_USER_CUSTOMER");
                GenericValue orderRole3 = delegator.makeValue("OrderRole", fields);
                toBeStored.add(orderRole3);
            }    
            partyRolePlacingCustomer = from("PartyRole").where("partyId", partyCustomerId, "roleTypeId", "PLACING_CUSTOMER").cache(true).queryOne();
            if (!partyRolePlacingCustomer) {
                fields = UtilMisc.toMap("orderId", orderId,"partyId", partyCustomerId, "roleTypeId", "PLACING_CUSTOMER");
                GenericValue orderRole4 = delegator.makeValue("OrderRole", fields);
                toBeStored.add(orderRole4);
            }    
            partyRoleShipToCustomer = from("PartyRole").where("partyId", partyCustomerId, "roleTypeId", "SHIP_TO_CUSTOMER").cache(true).queryOne();
            if (!partyRoleShipToCustomer) {
                fields = UtilMisc.toMap("orderId", orderId,"partyId", partyCustomerId, "roleTypeId", "SHIP_TO_CUSTOMER");
                GenericValue orderRole5 = delegator.makeValue("OrderRole", fields);
                toBeStored.add(orderRole5);
            }
    
            // Create OrderStatus
            String orderStatusId = "GEN_"+delegator.getNextSeqId("demo-orderstatusid");
            fields = UtilMisc.toMap("orderId", orderId,"orderStatusId",orderStatusId,"statusId","ORDER_CREATED",
                    "statusDatetime",orderDate,"statusUserLogin","admin");
            GenericValue orderStatus = delegator.makeValue("OrderStatus", fields);
            toBeStored.add(orderStatus);
        }

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