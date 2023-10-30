import com.ilscipio.scipio.ce.demoSuite.dataGenerator.AbstractDataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGeneratorProvider
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.util.EntityFindOptions
import org.ofbiz.entity.util.EntityUtil
import org.ofbiz.product.store.ProductStoreWorker

@DataGeneratorProvider(providers=[DataGeneratorProviders.LOCAL])
public class OrderData extends DataGeneratorGroovyBaseScript {
    private static final String module = "OrderData.groovy";
    
    OrderData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - ORDER DATA-=-=-=-", module);
    }

    public String getDataType() {
        return DataTypeEnum.ORDER;
    }

    public void init() {
        orderTypes = from("OrderType").getFieldList("orderTypeId");
        String orderType = (String) context.get("orderType");
        if (UtilValidate.isEmpty(orderType) || !orderTypes.contains(orderType)) {
            orderType = orderTypes.get(UtilRandom.random(orderTypes));
        }

        productStoreCount = from("ProductStore").queryCount();
        if (productStoreCount == 0) {
            throw new Exception("This service depends on product store data to be present. Please load or create product store data.");
        }
        totalProductStoreCount = (productStoreCount < Integer.MAX_VALUE) ? (int) productStoreCount : Integer.MAX_VALUE - 1;

        productCount = from("Product").queryCount();
        if (productCount == 0) {
            throw new Exception("This service depends on product data to be present. Please load product data or generate product demo data first and try again.");
        }
        totalProductCount = (productCount < Integer.MAX_VALUE) ? (int) productCount : Integer.MAX_VALUE - 1;

        partyGroupCount = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").queryCount();
        if (partyGroupCount == 0) {
            throw new Exception("This service depends on party group data to be present. Please load party group data or generate party group demo data first and try again.");
        }
        totalPartyGroupCount = (partyGroupCount < Integer.MAX_VALUE) ? (int) partyGroupCount : Integer.MAX_VALUE - 1;

        context.totalProductStoreCount = totalProductStoreCount;
        context.totalProductCount = totalProductCount;
        context.totalPartyGroupCount = totalPartyGroupCount;

        String prodCatalogId = context.prodCatalogId ?: null;
        String productStoreId = context.productStoreId ?: null;
        String partyGroupId = context.partyGroupId ?: null;
        String partyId = context.partyId ?: null;

        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);
        def customerParty = null;
        if (orderType.equals("SALES_ORDER")) {
            if (partyId) {
                customerPartyAndRole = from("PartyAndRole").where("partyId", partyId, "roleTypeId", "CUSTOMER").queryOne();
                if (customerPartyAndRole) {
                    customerParty = from("Party").where("partyId", partyId).queryOne();
                }
            }
            // If no partyId is passed or wrong customerPartyId, pick one randomly
            if (!customerParty) {
                partyCustomerCount = from("PartyRole").where("roleTypeId", "CUSTOMER").queryCount();
                if (partyCustomerCount == 0) {
                    throw new Exception("This service depends on party customer data to be present. Please load party customer data or generate party customer demo data first and try again.");
                }
                totalPartyCustomerCount = (partyCustomerCount < Integer.MAX_VALUE) ? (int) partyCustomerCount : Integer.MAX_VALUE - 1;
                context.totalPartyCustomerCount = totalPartyCustomerCount;

                efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyCustomerCount - 1));
                partyCustomerRoles = from("PartyRole").where("roleTypeId", "CUSTOMER").query(efo);
                if (partyCustomerRoles) {
                    customerParty = partyCustomerRoles[0].getRelatedOne("Party");
                }
            }
            if (!customerParty) {
                throw new Exception("Party customer not found or invalid.");
            }
            customerUserLogin = EntityUtil.getFirst(customerParty.getRelated("UserLogin"));
            context.customerParty = customerParty;
            context.customerUserLogin = customerUserLogin;
        } else {
            supplierParty = null;
            if (partyId) {
                supplierParty = from("Party").where("PartyRole").where("partyId", partyId, "roleTypeId", "SUPPLIER").queryOne();
            }
            // If no partyId is passed or wrong supplierPartyId, pick one randomly
            if (!supplierParty) {
                partySupplierCount = from("PartyRole").where("roleTypeId", "SUPPLIER").queryCount();
                if (partySupplierCount == 0) {
                    throw new Exception("This service depends on party supplier data to be present. Please load party supplier data.");
                }
                totalPartySupplierCount = (partySupplierCount < Integer.MAX_VALUE) ? (int) partySupplierCount : Integer.MAX_VALUE - 1;
                context.totalPartySupplierCount = totalPartySupplierCount;

                efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartySupplierCount - 1));
                partySupplierRoles = from("PartyRole").where("roleTypeId", "SUPPLIER").query(efo);
                if (partySupplierRoles ) {
                    supplierParty = partySupplierRoles[0].getRelatedOne("Party");
                }
            }
            if (!supplierParty) {
                throw new Exception("Party customer not found or invalid.");
            }
            supplierUserLogin = (customerParty != null) ? EntityUtil.getFirst(customerParty.getRelated("UserLogin")) : null;
            context.supplierParty = supplierParty;
            context.supplierUserLogin = supplierUserLogin;
        }

        // Check if we got a valid ProductStore
        if (!productStoreId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalProductStoreCount - 1));
            productStores = from("ProductStore").query(efo);
            if (productStores) {
                productStoreId = productStores[0].productStoreId;
            }
            if (!prodCatalogId && productStoreId) {
                productStoreCatalog = EntityUtil.getFirst(EntityUtil.filterByDate(from("ProductStoreCatalog").where("productStoreId", productStoreId).queryList()));
                prodCatalogId = productStoreCatalog.getString("prodCatalogId");
            }
        } else {
            if (!ProductStoreWorker.getProductStore(productStoreId, delegator)) {
                productStoreId = null;
            }
        }
        if (!productStoreId) {
            throw new Exception("Product store not found or invalid.");
        }

        // Check if we got a valid WebSite
        webSiteCount = from("WebSite").where("productStoreId", productStoreId, "visualThemeSetId", "ECOMMERCE").queryCount();
        totalWebSiteCount = (webSiteCount  < Integer.MAX_VALUE) ? (int) webSiteCount : Integer.MAX_VALUE - 1;
        efo.setOffset(UtilRandom.getRandomInt(0, totalWebSiteCount - 1));
        webSites = from("WebSite").query(efo);
        if (!webSites) {
            throw new Exception("Website not found or invalid.");
        }
        context.totalWebSiteCount = totalWebSiteCount;
        context.webSite = webSites.get(0);

        // If no partyGroupId is passed, pick one randomly
        if (!partyGroupId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyGroupCount - 1));
            partyGroups = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").query(efo);
            if (partyGroups) {
                partyGroupId = partyGroups[0].getString("partyId");
            }
        }
        if (!partyGroupId) {
            throw new Exception("Party group not found or invalid.");
        }

        context.prodCatalogId = prodCatalogId;
        context.productStoreId = productStoreId;
        context.partyGroupId = partyGroupId;
        context.partyId = partyId;
    }

    List prepareData(int index, AbstractDataObject orderData) throws Exception {
        List toBeStored = UtilMisc.newList();
        List orderItems = UtilMisc.newList();

        AbstractDataGenerator generator = context.generator;
        if (orderData) {
            for (orderItem in orderData.getOrderItems()) {
                Map<String, Object> orderItemFields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderItemSeqId", orderItem.getOrderItemId(), "orderItemTypeId", orderItem.getOrderItemType(), 
                        "productId", orderItem.getProductId(), "prodCatalogId", context.prodCatalogId, "isPromo", orderItem.getIsPromo(), "quantity", orderItem.getQuantity(), "selectedAmount", orderItem.getSelectedAmount(),
                        "unitPrice", orderItem.getUnitPrice(), "unitListPrice", orderItem.getUnitListPrice(), "isModifiedPrice","N", "statusId","ITEM_APPROVED");
                orderItems.add(delegator.makeValue("OrderItem", orderItemFields));
            }

            Map orderHeaderFields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderTypeId", orderData.getOrderType(), "orderName", orderData.getOrderName(), "salesChannelEnumId",
                    orderData.getChannel(), "orderDate", orderData.getOrderDate(), "priority", "2", "entryDate", orderData.getOrderDate(), "statusId", "ORDER_CREATED",
                    "currencyUom", "USD", "webSiteId", context.webSite.getString("webSiteId"), "remainingSubTotal", orderData.getRemainingSubTotal(), "grandTotal", orderData.getGrandTotal(),
                    "productStoreId", context.productStoreId, "createdBy", orderData.getCreatedBy());

            toBeStored.add(delegator.makeValue("OrderHeader", orderHeaderFields));
            toBeStored.addAll(orderItems);

            for (orderRole in orderData.getOrderRoles()) {
                Map<String, Object> partyRoleFields = UtilMisc.toMap("partyId", orderRole.getUserId(), "roleTypeId", orderRole.getRoleType());
                toBeStored.add(delegator.makeValue("PartyRole", partyRoleFields));
                Map<String, Object> orderRoleFields = UtilMisc.toMap("orderId", orderData.getOrderId(), "partyId", orderRole.getUserId(), "roleTypeId", orderRole.getRoleType());
                toBeStored.add(delegator.makeValue("OrderRole", orderRoleFields));
            }

            for (orderStatus in orderData.getOrderStatuses()) {
                Map<String, Object> fields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderStatusId", orderStatus.getOrderStatusId(),
                        "statusId", orderStatus.getStatusId(), "statusDatetime", orderStatus.getStatusDate());
                //, "statusUserLogin", "admin");
                toBeStored.add(delegator.makeValue("OrderStatus", fields));
            }

            for (orderContactMech in orderData.getOrderContactMechs()) {
                Map<String, Object> fields = UtilMisc.toMap("orderId", orderData.getOrderId(), "contactMechPurposeTypeId", orderContactMech.getContactMechPurposeTypeId(),
                        "contactMechId", orderContactMech.getContactMechId());
                toBeStored.add(delegator.makeValue("OrderContactMech", fields));
            }
        }


        return toBeStored;
    }
}