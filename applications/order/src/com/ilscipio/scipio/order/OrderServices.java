package com.ilscipio.scipio.order;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.LocalService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import java.sql.Timestamp;
import java.util.*;

public abstract class OrderServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "OrderUiLabels";
    public static final String resource_error = "OrderErrorUiLabels";
    public static final String resourceProduct = "ProductUiLabels";
    public static final String resourceCommon = "CommonUiLabels";

    protected OrderServices() {}

    @Service(
            description = "Populates a best-selling category with products based on sales total or quantity ordered"
    )
    @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "false")
    @Attribute(name = "orderByType", type = "String", mode = "IN", optional = "true", defaultValue = "quantity-ordered",
            description = "Ordering criteria, one of (extensible by overriding services): " +
                    "sales-total: use BestSellingProductsBySalesTotal view-entity or equivalent; " +
                    "quantity-ordered: use BestSellingProductsByQuantityOrdered view-entity or equivalent; " +
                    "order-item-count: use BestSellingProductsByQuantityOrdered view-entity or equivalent")
    @Attribute(name = "productStoreIdList", type = "List", mode = "IN", optional = "true",
            description = "Filter for OrderHeader.productStoreId")
    @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true",
            description = "Filter for OrderHeader.productStoreId")
    @Attribute(name = "filterCategoryId", type = "String", mode = "IN", optional = "true",
            description = "Only products belonging to this category and sold from this category are considered, using OrderItem.productCategoryId")
    @Attribute(name = "filterCategoryIdWithParents", type = "String", mode = "IN", optional = "true",
            description = "Only products belonging to this category or whose (virtual) parents are in the category or are considered")
    @Attribute(name = "filterSalesDiscDate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true")
    @Attribute(name = "maxProducts", type = "Integer", mode = "IN", optional = "true")
    @Attribute(name = "orderDateStart", type = "Timestamp", mode = "IN", optional = "true",
            description = "Absolute start date if specified")
    @Attribute(name = "orderDateEnd", type = "Timestamp", mode = "IN", optional = "true",
            description = "Absolute end date if specified; defaults to now")
    @Attribute(name = "orderDateDays", type = "Integer", mode = "IN", optional = "true", defaultValue = "30",
            description = "Days to substract from orderDateEnd to get absolute start date")
    @Attribute(name = "updateMode", type = "String", mode = "IN", optional = "true", defaultValue = "update",
            description = "Update mode, one of: " +
                    "update: update records in-place where necessary and possible; " +
                    "create: always delete and recreate records (makes records sequenced by date)")
    @Attribute(name = "removeMode", type = "String", mode = "IN", optional = "true", defaultValue = "remove",
            description = "Removal mode, one of: " +
                    "remove: remove old records; " +
                    "preserve: ignore old records (faster)")
    @Attribute(name = "logEvery", type = "Integer", mode = "IN", optional = "true", defaultValue = "50")
    public static class PopulateBestSellingCategory extends LocalService {
        protected static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        protected String productCategoryId;
        protected GenericValue productCategory;
        protected int productCount = 0;
        protected int removed = 0;
        protected int created = 0;
        protected int updated = 0;
        protected int maxTopProductIds = 10;
        protected Set<String> topProductIds = new LinkedHashSet<>();
        protected Collection<String> productStoreIds = null;
        protected long sequenceNum = 1;
        protected Timestamp orderDateStart;
        protected Timestamp orderDateEnd;
        protected Integer orderDateDays;
        protected String filterCategoryId;
        protected String filterCategoryIdWithParents;
        protected Timestamp nowTimestamp;
        protected boolean removeOld;
        protected boolean createNew;
        protected boolean filterSalesDiscDate;
        protected Integer logEvery;

        @Override
        public void init(ServiceContext ctx) throws GeneralException {
            super.init(initServiceLogNew(ctx, module));
            if (nowTimestamp == null) {
                nowTimestamp = UtilDateTime.nowTimestamp();
            }
            productCategoryId = ctx.attr("productCategoryId");
            productStoreIds = ctx.attrNonEmpty("productStoreIdList");
            String productStoreId = ctx.getStringNonEmpty("productStoreId");
            if (UtilValidate.isNotEmpty(productStoreId)) {
                Set<String> newProductStoreIds = new LinkedHashSet<>();
                newProductStoreIds.add(productStoreId);
                if (UtilValidate.isNotEmpty(productStoreIds)) {
                    newProductStoreIds.addAll(productStoreIds);
                }
                productStoreIds = newProductStoreIds;
            }
            orderDateStart = ctx.attr("orderDateStart");
            orderDateEnd = ctx.attr("orderDateEnd"); // don't force here: UtilDateTime::nowTimestamp
            orderDateDays = ctx.attr("orderDateDays");
            if (orderDateStart == null && orderDateDays != null && orderDateDays > 0) {
                if (orderDateEnd == null) {
                    orderDateEnd = nowTimestamp;
                }
                orderDateStart = UtilDateTime.addDaysToTimestamp(orderDateEnd, -orderDateDays);
            }
            filterCategoryId = ctx.attr("filterCategoryId");
            filterCategoryIdWithParents = ctx.attr("filterCategoryIdWithParents");
            removeOld = "remove".equals(ctx.attr("removeMode"));
            createNew = "create".equals(ctx.attr("updateMode"));
            filterSalesDiscDate = ctx.attr("filterSalesDiscDate", true);
            logEvery = ctx.attr("logEvery");
        }

        @Override
        public Map<String, Object> exec() throws ServiceValidationException {
            try {
                productCategory = ctx.delegator().from("ProductCategory").where("productCategoryId", productCategoryId).queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, srvModule);
                return ServiceUtil.returnError(e.toString());
            }
            if (productCategory == null) {
                return ServiceUtil.returnError("ProductCategory [" + productCategoryId + "] not found");
            }

            Set<String> unseenProductIds = null;
            if (removeOld) {
                if (createNew) {
                    // Remove old immediately
                    Iterator<GenericValue> catIt = null;
                    try {
                        // Might not trigger all needed ECAs, so do one-by one...
                        //int removed = ctx.delegator().removeByAnd("ProductCategoryMember", "productCategoryId", productCategoryId);
                        catIt = UtilMisc.asIterator(getCategoryMembers(productCategoryId));
                        GenericValue pcm;
                        while((pcm = UtilMisc.next(catIt)) != null) {
                            removeProductCategoryMember(pcm);
                        }
                        Debug.logInfo("Removed " + removed + " old ProductCategoryMember records for category [" +
                                productCategoryId + "]", srvModule);
                    } catch (GeneralException e) {
                        Debug.logError(e, srvModule);
                        return ServiceUtil.returnError(e.toString());
                    } finally {
                        if (catIt instanceof AutoCloseable) {
                            try {
                                ((AutoCloseable) catIt).close();
                            } catch (Exception e) {
                                Debug.logError(e, srvModule);
                            }
                        }
                    }
                } else {
                    // Delayed remove
                    try {
                        unseenProductIds = getCategoryProductIdSet(productCategoryId);
                    } catch (GeneralException e) {
                        Debug.logError(e, srvModule);
                        return ServiceUtil.returnError(e.toString());
                    }
                }
            }

            Iterator<? extends Map<String, Object>> prodIt = null;
            try {
                Debug.logInfo("Beginning query on category [" + productCategoryId + "]", srvModule);
                prodIt = UtilMisc.asIterator(getProducts());
                Integer maxProducts = ctx.attr("maxProducts");
                Map<String, Object> productEntry;
                while (((productEntry = UtilMisc.next(prodIt)) != null) && (maxProducts == null || productCount < maxProducts)) {
                    String productId = (String) productEntry.get("productId");
                    ProductInfo info = makeProductInfo(productEntry, sequenceNum);
                    if (!useProduct(info)) {
                        continue;
                    }
                    GenericValue pcm = createUpdateProductCategoryMember(info);
                    if (unseenProductIds != null) {
                        unseenProductIds.remove(productId);
                    }
                    sequenceNum++;
                    if (pcm != null) {
                        productCount++;
                    }
                    if (logEvery != null && (sequenceNum % logEvery == 0)) {
                        Debug.logInfo("Visited " + sequenceNum + " records, added " + productCount + " products", srvModule);
                    }
                }
            } catch(GeneralException e) {
                Debug.logError(e, srvModule);
                return ServiceUtil.returnError(e.toString());
            } finally {
                if (prodIt instanceof AutoCloseable) {
                    try {
                        ((AutoCloseable) prodIt).close();
                    } catch (Exception e) {
                        Debug.logError(e, srvModule);
                    }
                }
            }

            if (removeOld && UtilValidate.isNotEmpty(unseenProductIds)) {
                Iterator<GenericValue> catIt = null;
                try {
                    catIt = UtilMisc.asIterator(getCategoryMembers(productCategoryId, unseenProductIds));
                    GenericValue pcm;
                    while((pcm = UtilMisc.next(catIt)) != null) {
                        removeProductCategoryMember(pcm);
                    }
                } catch (GeneralException e) {
                    Debug.logError(e, srvModule);
                    return ServiceUtil.returnError(e.toString());
                } finally {
                    if (catIt instanceof AutoCloseable) {
                        try {
                            ((AutoCloseable) catIt).close();
                        } catch (Exception e) {
                            Debug.logError(e, srvModule);
                        }
                    }
                }
            }

            String msg = getSuccessMsg();
            Debug.logInfo("populateBestSellingCategory: " + msg, srvModule);
            return ServiceUtil.returnSuccess(msg);
        }

        /**
         * Returns iterator or collection of Map product entries.
         */
        protected Object getProducts() throws GeneralException {
            String orderByType = ctx.attr("orderByType");
            if ("quantity-ordered".equals(orderByType)) {
                return getProductsByQuantityOrdered();
            } else if ("sales-total".equals(orderByType)) {
                return getProductsBySalesTotal();
            } else if ("order-item-count".equals(orderByType)) {
                return getProductsByOrderItemCount();
            //} else if ("solr".equals(orderByType)) {
            //    return getProductsBySolr();
            } else {
                throw new ServiceValidationException("Invalid orderByType", ctx.getModelService());
            }
        }

        protected Object getProductsBySalesTotal() throws GeneralException {
            DynamicViewEntity dve = ctx.delegator().makeDynamicViewEntity("BestSellingProductsBySalesTotal");
            EntityCondition cond = makeCommonCondition();
            cond = addFilterCategoryIdCond(cond, dve);
            return ctx.delegator().from(dve).where(cond).orderBy("-salesTotal").cache(false).queryIterator();
        }

        protected Object getProductsByQuantityOrdered() throws GeneralException {
            DynamicViewEntity dve = ctx.delegator().makeDynamicViewEntity("BestSellingProductsByQuantityOrdered");
            EntityCondition cond = makeCommonCondition();
            cond = addFilterCategoryIdCond(cond, dve);
            return ctx.delegator().from(dve).where(cond).orderBy("-quantityOrdered").cache(false).queryIterator();
        }

        protected Object getProductsByOrderItemCount() throws GeneralException {
            DynamicViewEntity dve = ctx.delegator().makeDynamicViewEntity("BestSellingProductsByOrderItemCount");
            EntityCondition cond = makeCommonCondition();
            cond = addFilterCategoryIdCond(cond, dve);
            return ctx.delegator().from(dve).where(cond).orderBy("-orderItemCount").cache(false).queryIterator();
        }

        protected EntityCondition makeCommonCondition() throws GeneralException {
            EntityCondition cond = EntityCondition.makeDateRangeCondition("orderDate", orderDateStart, orderDateEnd);
            if (UtilValidate.isNotEmpty(productStoreIds)) {
                cond = EntityCondition.combine(cond,
                        EntityCondition.makeCondition("productStoreId", EntityOperator.IN, productStoreIds));
            }
            return cond;
        }

        protected EntityCondition addFilterCategoryIdCond(EntityCondition cond, DynamicViewEntity dve) {
            if (filterCategoryId != null) {
                if (dve != null) {
                    dve.addMemberEntity("PCM", "ProductCategoryMember");
                    dve.addAlias("PCM", "filterCategoryId", "productCategoryId", false, null, false);
                    dve.addViewLink("OI", "PCM", false,
                            UtilMisc.toList(new ModelKeyMap("productId", "productId")));
                }
                EntityCondition filterCond = EntityCondition.makeCondition("filterCategoryId", filterCategoryId);
                cond = EntityCondition.makeCondition(cond, EntityOperator.AND, filterCond);
            }
            return cond;
        }

        protected Object getCategoryMembers(String productCategoryId) throws GeneralException {
            return ctx.delegator().from("ProductCategoryMember").where("productCategoryId", productCategoryId)
                    .queryIterator();
        }

        protected Set<String> getCategoryProductIdSet(String productCategoryId) throws GeneralException {
            return ctx.delegator().from("ProductCategoryMember").where("productCategoryId", productCategoryId)
                    .getFieldSet("productId");
        }

        protected Object getCategoryMembers(String productCategoryId, Collection<String> productIdList) throws GeneralException {
            return ctx.delegator().from("ProductCategoryMember").where(
                    EntityCondition.makeCondition("productCategoryId", productCategoryId),
                    EntityCondition.makeCondition("productId", EntityOperator.IN, productIdList))
                    .queryIterator();
        }

        /**
         * Returns null if none created/skip.
         */
        protected GenericValue createUpdateProductCategoryMember(ProductInfo info) throws GeneralException {
            List<GenericValue> prevPcms = ctx.delegator().from("ProductCategoryMember")
                    .where("productCategoryId", productCategoryId, "productId", info.getProductId())
                    .filterByDate(nowTimestamp).queryList();
            GenericValue pcm;
            if (UtilValidate.isNotEmpty(prevPcms)) {
                if (prevPcms.size() > 1) {
                    Debug.logWarning("ProductCategoryMember productCategoryId [" + productCategoryId +
                            "] productId [" + info.getProductId() + "] has more than one (" + prevPcms.size() +
                            ") entry for category; updating first only", srvModule);
                }
                pcm = prevPcms.get(0);
                pcm.set("sequenceNum", info.getSequenceNum());
                pcm.store();
                updated++;
            } else {
                pcm = ctx.delegator().makeValue("ProductCategoryMember",
                        "productCategoryId", productCategoryId,
                        "productId", info.getProductId(),
                        "fromDate", nowTimestamp,
                        "sequenceNum", info.getSequenceNum()).create();
                created++;
            }
            if (topProductIds.size() < maxTopProductIds) {
                topProductIds.add(info.getProductId());
            }
            return pcm;
        }

        /** Applies individual product filters; may be overridden. */
        protected boolean useProduct(ProductInfo info) throws GeneralException {
            if (filterCategoryIdWithParents != null && !hasCategoryIdWithParents(info)) {
                return false;
            }
            if (filterSalesDiscDate && !isOkSalesDiscDate(info)) {
                return false;
            }
            return true;
        }

        protected boolean hasCategoryIdWithParents(ProductInfo info) throws GeneralException {
            return hasCategoryIdWithParents(info.getProductId(), info.getProduct(), filterCategoryIdWithParents);
        }

        protected boolean hasCategoryIdWithParents(String productId, Map<String, Object> product,
                                                   String filterCatId) throws GeneralException {
            if (ctx.delegator().from("ProductCategoryMember")
                    .where("productCategoryId", filterCatId, "productId", productId)
                    .filterByDate(nowTimestamp).queryCount() > 0) {
                return true;
            }
            if (product == null) {
                product = ctx.delegator().from("Product").where("productId", productId).queryOne();
                if (product == null) {
                    Debug.logError("Could not find product [" + productId + "]", srvModule);
                    return false;
                }
            }
            String parentProductId = getParentProductId(productId, product);
            if (parentProductId != null) {
                return hasCategoryIdWithParents(parentProductId, null, filterCatId);
            }
            return false;
        }

        protected boolean isOkSalesDiscDate(ProductInfo info) throws GeneralException {
            Timestamp salesDiscontinuationDate = info.getProduct().getTimestamp("salesDiscontinuationDate");
            if (salesDiscontinuationDate != null) {
                return !salesDiscontinuationDate.before(nowTimestamp);
            };
            return true;
        }

        protected String getParentProductId(String productId, Map<String, Object> product) throws GeneralException {
            Object isVariant = product.get("isVariant");
            if ((isVariant instanceof Boolean && ((Boolean) isVariant)) || "Y".equals(isVariant)) {
                GenericValue parentProductAssoc = ProductWorker.getParentProductAssoc(productId, ctx.delegator(),
                        nowTimestamp, false);
                if (parentProductAssoc != null) {
                    return parentProductAssoc.getString("productId");
                }
            }
            return null;
        }

        protected void removeProductCategoryMember(GenericValue pcm) throws GeneralException {
            pcm.remove();
            removed++;
        }

        protected String getSuccessMsg() {
            return "Category: " + productCategoryId + ": " + productCount + " products found; " + created +
                    " members created; " + updated + " members updated; " + removed +
                    " members removed or recreated; top products: " + topProductIds;
        }

        protected ProductInfo makeProductInfo(Map<String, Object> productEntry, long sequenceNum) {
            return new ProductInfo(productEntry, sequenceNum);
        }

        protected class ProductInfo {
            /** Usually a view-entity value, NOT Product */
            protected Map<String, Object> productEntry;
            protected String productId;
            protected long sequenceNum;
            protected GenericValue product;

            protected ProductInfo(Map<String, Object> productEntry, long sequenceNum) {
                this.productEntry = productEntry;
                this.productId = (String) productEntry.get("productId");
                this.sequenceNum = sequenceNum;
            }

            public Map<String, Object> getProductEntry() {
                return productEntry;
            }

            public String getProductId() {
                return productId;
            }

            public long getSequenceNum() {
                return sequenceNum;
            }

            public GenericValue getProduct() throws GenericEntityException {
                GenericValue product = this.product;
                if (product == null) {
                    product = ctx.delegator().from("Product").where("productId", getProductId()).queryOne();
                    this.product = product;
                }
                return product;
            }
        }
    }
}
