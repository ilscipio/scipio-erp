import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.product.store.ProductStoreWorker

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.AbstractDataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGeneratorProvider
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders


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
        productStoreCount = from("ProductStore").queryCount();
        if (productStoreCount == 0) {
            throw new Exception("This service depends on product store data to be present. Please load or create product store data.");
        }
        totalProductStoreCount = (productStoreCount  < Integer.MAX_VALUE) ? (int) productStoreCount : Integer.MAX_VALUE - 1;

        productCount = from("Product").queryCount();
        if (productCount == 0) {
            throw new Exception("This service depends on product data to be present. Please load product data or generate product demo data first and try again.");
        }
        totalProductCount = (productCount < Integer.MAX_VALUE) ? (int) productCount : Integer.MAX_VALUE - 1;

        partyGroupCount = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").queryCount();
        if (partyGroupCount == 0) {
            throw new Exception("This service depends on party group data to be present. Please load party group data or generate party group demo data first and try again.");
        }
        totalPartyGroupCount = (partyGroupCount  < Integer.MAX_VALUE) ? (int) partyGroupCount : Integer.MAX_VALUE - 1;

        partyCustomerCount = from("PartyRole").where("roleTypeId", "CUSTOMER").queryCount();
        if (partyCustomerCount == 0) {
            throw new Exception("This service depends on party customer data to be present. Please load party customer data or generate party customer demo data first and try again.");
        }
        totalPartyCustomerCount = (partyCustomerCount  < Integer.MAX_VALUE) ? (int) partyCustomerCount : Integer.MAX_VALUE - 1;

        context.totalProductStoreCount = totalProductStoreCount;
        context.totalProductCount = totalProductCount;
        context.totalPartyGroupCount = totalPartyCustomerCount;
        context.totalPartyCustomerCount = totalPartyCustomerCount;

        String prodCatalogId = context.prodCatalogId ?: null;
        String productStoreId = context.productStoreId ?: null;
        String partyGroupId = context.partyGroupId ?: null;
        String partyCustomerId = context.partyCustomerId ?: null;

        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);

        // Check if we got a valid ProductStore
        if (!productStoreId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalProductStoreCount - 1));
            //            Debug.log("productStoreId offset ======> " + efo.getOffset());
            productStores = from("ProductStore").query(efo);
            if (productStores) {
                productStoreId = productStores[0].productStoreId;
            }
            if (!prodCatalogId && productStoreId) {
                productStoreCatalog = EntityUtil.getFirst(EntityUtil.filterByDate(from("ProductStoreCatalog").where("productStoreId", productStoreId).queryList()));
                prodCatalogId = productStoreCatalog.getString("prodCatalogId");
            }
        } else {
            if (!ProductStoreWorker.getProductStore(productStoreId)) {
                productStoreId = null;
            }
        }
        if (!productStoreId) {
            throw new Exception("Product store not found or invalid.");
        }

        // Check if we got a valid WebSite
        GenericValue webSite = from("WebSite").where("productStoreId", productStoreId).queryFirst();
        if (!webSite) {
            throw new Exception("Website not found or invalid.");
        }

        // If no partyGroupId is passed, pick one randomly
        if (!partyGroupId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyGroupCount - 1));
            //            Debug.log("party group offset ======> " + efo.getOffset());
            partyGroups = from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").query(efo);
            if (partyGroups) {
                partyGroupId = partyGroups[0].getString("partyId");
            }
        }
        if (!partyGroupId) {
            throw new Exception("Party group not found or invalid.");
        }

        // If no partyCustomerId is passed, pick one randomly
        if (!partyCustomerId) {
            efo.setOffset(UtilRandom.getRandomInt(0, context.totalPartyCustomerCount - 1));
            //            Debug.log("party customer offset ======> " + efo.getOffset());
            partyCustomers = from("PartyRole").where("roleTypeId", "CUSTOMER").query(efo);
            if (partyCustomers) {
                partyCustomerId = partyCustomers[0].getString("partyId");
            }
        }
        if (!partyCustomerId) {
            throw new Exception("Party customer not found or invalid.");
        }

        context.prodCatalogId = prodCatalogId;
        context.productStoreId = productStoreId;
        context.partyGroupId = partyGroupId;
        context.partyCustomerId = partyCustomerId;
        context.webSite = webSite;
    }

    List prepareData(int index, AbstractDataObject orderData) throws Exception {
        List toBeStored = new ArrayList();
        List orderItems = new ArrayList();

        AbstractDataGenerator generator = context.generator;
        if (orderData) {
            for (orderItem in orderData.getOrderItems()) {
                Map<String, Object> orderItemFields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderItemSeqId", orderItem.getOrderItemId(), "orderItemTypeId", orderItem.getOrderItemType(), 
                        "productId", orderItem.getProductId(), "prodCatalogId", context.prodCatalogId, "isPromo", orderItem.getIsPromo(), "quantity", orderItem.getQuantity(), "selectedAmount", orderItem.getSelectedAmount(),
                        "unitPrice", orderItem.getUnitPrice(), "unitListPrice", orderItem.getUnitListPrice(), "isModifiedPrice","N", "itemDescription", "Round Gizmo", "statusId","ITEM_APPROVED");
                orderItems.add(delegator.makeValue("OrderItem", orderItemFields));
            }

            Map orderHeaderFields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderTypeId", orderData.getOrderType(), "orderName", orderData.getOrderName(), "salesChannelEnumId",
                    orderData.getChannel(), "orderDate", orderData.getOrderDate(), "priority", "2", "entryDate", orderData.getOrderDate(), "statusId", "ORDER_CREATED",
                    "currencyUom", "USD", "webSiteId", context.webSite.getString("webSiteId"), "remainingSubTotal", orderData.getRemainingSubTotal(), "grandTotal", orderData.getGrandTotal(),
                    "productStoreId", context.productStoreId);

            toBeStored.add(delegator.makeValue("OrderHeader", orderHeaderFields));
            toBeStored.addAll(orderItems);

            for (orderRole in orderData.getOrderRoles()) {
                Map<String, Object> fields = UtilMisc.toMap("orderId", orderData.getOrderId(), "partyId", orderRole.getUserId(), "roleTypeId", orderRole.getRoleType());
                toBeStored.add(delegator.makeValue("OrderRole", fields));
            }

            for (orderStatus in orderData.getOrderStatuses()) {
                Map<String, Object> fields = UtilMisc.toMap("orderId", orderData.getOrderId(), "orderStatusId", orderStatus.getOrderStatusId(),
                        "statusId", orderStatus.getStatusId(), "statusDatetime", orderStatus.getStatusDate());
                //, "statusUserLogin", "admin");
                toBeStored.add(delegator.makeValue("OrderStatus", fields));
            }
        }


        return toBeStored;
    }
}