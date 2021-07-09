package com.ilscipio.scipio.order;

import org.ofbiz.base.util.AbortException;
import org.ofbiz.base.util.ContinueException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceHandler;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class OrderServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "OrderUiLabels";
    public static final String resource_error = "OrderErrorUiLabels";
    public static final String resourceProduct = "ProductUiLabels";
    public static final String resourceCommon = "CommonUiLabels";

    protected OrderServices() {}

    /**
     * Implements populateBestSellingCategory service; service overrides can override any protected methods.
     */
    public static class PopulateBestSellingCategory extends ServiceHandler.Local {
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

        public PopulateBestSellingCategory(ServiceContext ctx) throws GeneralException {
            super(ctx);
            this.productCategoryId = ctx.attr("productCategoryId");
            Collection<String> productStoreIds = ctx.attr("productStoreIdList");
            String productStoreId = ctx.getStringNonNull("productStoreIds");
            if (UtilValidate.isNotEmpty(productStoreId)) {
                Set<String> newProductStoreIds = new LinkedHashSet<>();
                newProductStoreIds.add(productStoreId);
                if (UtilValidate.isNotEmpty(productStoreIds)) {
                    newProductStoreIds.addAll(productStoreIds);
                }
                productStoreIds = newProductStoreIds;
            }
            this.productStoreIds = UtilValidate.isNotEmpty(productStoreIds) ? productStoreIds : null;
        }

        public Map<String, Object> exec() throws ServiceValidationException {
            boolean removeOld = "remove".equals(ctx.attr("removeMode"));
            boolean createNew = "create".equals(ctx.attr("updateMode"));

            try {
                productCategory = ctx.delegator().from("ProductCategory").where("productCategoryId", productCategoryId).queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
            if (productCategory == null) {
                return ServiceUtil.returnError("ProductCategory [" + productCategoryId + "] not found");
            }

            Set<String> unseenProductIds = null;
            if (removeOld) {
                if (createNew) {
                    // Remove old immediately
                    try {
                        // Might not trigger all needed ECAs, so do one-by one...
                        //int removed = ctx.delegator().removeByAnd("ProductCategoryMember", "productCategoryId", productCategoryId);
                        try(EntityListIterator eli = makeCategoryMembersQuery(productCategoryId).queryIterator()) {
                            GenericValue pcm;
                            while((pcm = eli.next()) != null) {
                                removeProductCategoryMember(pcm);
                            }
                        }
                        Debug.logInfo("Removed " + removed + " old ProductCategoryMember records for category [" +
                                productCategoryId + "]", module);
                    } catch (GeneralException e) {
                        Debug.logError(e, module);
                        return ServiceUtil.returnError(e.toString());
                    }
                } else {
                    // Delayed remove
                    try {
                        unseenProductIds = makeCategoryMembersQuery(productCategoryId).getFieldSet("productId");
                    } catch (GeneralException e) {
                        Debug.logError(e, module);
                        return ServiceUtil.returnError(e.toString());
                    }
                }
            }

            try {
                try (EntityListIterator eli = makeProductsQuery().queryIterator()) {
                    GenericValue productStatsValue;
                    Integer maxProducts = ctx.attr("maxProducts");
                    while (((productStatsValue = eli.next()) != null) && (maxProducts == null || productCount < maxProducts)) {
                        String productId = productStatsValue.getString("productId");
                        GenericValue pcm = createUpdateProductCategoryMember(productCategoryId, productId, sequenceNum, productStatsValue);
                        if (unseenProductIds != null) {
                            unseenProductIds.remove(productId);
                        }
                        sequenceNum++;
                        if (pcm != null) {
                            productCount++;
                        }
                    }
                }
            } catch(GeneralException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }

            if (removeOld && UtilValidate.isNotEmpty(unseenProductIds)) {
                try {
                    try(EntityListIterator eli = makeCategoryMembersQuery(productCategoryId, unseenProductIds).queryIterator()) {
                        GenericValue pcm;
                        while((pcm = eli.next()) != null) {
                            removeProductCategoryMember(pcm);
                        }
                    }
                } catch (GeneralException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }
            }
            String msg = "Category: " + productCategoryId + ": " + productCount + " products found; " + created +
                    " members created; " + updated + " members updated; " + removed +
                    " members removed or recreated; top products: " + topProductIds;
            Debug.logInfo("populateBestSellingCategory: " + msg, module);
            return ServiceUtil.returnSuccess(msg);
        }

        protected EntityQuery makeProductsQuery() throws GeneralException {
            String orderByType = ctx.attr("orderByType");
            if ("quantity-ordered".equals(orderByType)) {
                return makeQuantityOrderedQuery();
            } else if ("sales-total".equals(orderByType)) {
                return makeSalesTotalQuery();
            } else {
                throw new ServiceValidationException("Invalid orderByType", ctx.getModelService());
            }
        }

        protected EntityCondition makeCommonCondition() throws GeneralException {
            EntityCondition cond = EntityCondition.makeDateRangeCondition("orderDate",
                    ctx.attr("orderDateStart"), ctx.attr("orderDateEnd"));
            if (UtilValidate.isNotEmpty(productStoreIds)) {
                cond = EntityCondition.combine(cond,
                        EntityCondition.makeCondition("productStoreId", EntityOperator.IN, productStoreIds));
            }
            return cond;
        }

        protected EntityQuery makeSalesTotalQuery() throws GeneralException {
            // NOTE: This used to be here but it becomes constraining: .maxRows(ctx.attr("maxProducts"));
            return ctx.delegator().from("BestSellingProductsBySalesTotal")
                    .where(makeCommonCondition())
                    .orderBy("-salesTotal").cache(false);
        }

        protected EntityQuery makeQuantityOrderedQuery() throws GeneralException {
            // NOTE: This used to be here but it becomes constraining: .maxRows(ctx.attr("maxProducts"));
            return ctx.delegator().from("BestSellingProductsByQuantityOrdered")
                    .where(makeCommonCondition())
                    .orderBy("-quantityOrdered").cache(false);
        }

        protected EntityQuery makeCustomQuery() throws GeneralException {
            throw new ServiceValidationException("Invalid orderByType for query", ctx.getModelService());
        }

        protected EntityQuery makeCategoryMembersQuery(String productCategoryId) throws GeneralException {
            return ctx.delegator().from("ProductCategoryMember").where("productCategoryId", productCategoryId);
        }

        protected EntityQuery makeCategoryMembersQuery(String productCategoryId, Collection<String> productIdList) throws GeneralException {
            return ctx.delegator().from("ProductCategoryMember").where(
                    EntityCondition.makeCondition("productCategoryId", productCategoryId),
                    EntityCondition.makeCondition("productId", EntityOperator.IN, productIdList));
        }

        /**
         * Returns null if none created/skip.
         */
        protected GenericValue createUpdateProductCategoryMember(String productCategoryId, String productId, long sequenceNum,
                                                                 GenericValue productStatsValue) throws GeneralException {
            List<GenericValue> prevPcms = ctx.delegator().from("ProductCategoryMember")
                    .where("productCategoryId", productCategoryId, "productId", productId, "thruDate", null).queryList();
            GenericValue pcm;
            if (UtilValidate.isNotEmpty(prevPcms)) {
                if (prevPcms.size() > 1) {
                    Debug.logWarning("ProductCategoryMember productCategoryId [" + productCategoryId +
                            "] productId [" + productId + "] has more than one (" + prevPcms.size() +
                            ") entry for category; updating first only", module);
                }
                pcm = prevPcms.get(0);
                pcm.set("sequenceNum", sequenceNum);
                pcm.store();
                updated++;
            } else {
                pcm = ctx.delegator().makeValue("ProductCategoryMember",
                        "productCategoryId", productCategoryId,
                        "productId", productId,
                        "fromDate", UtilDateTime.nowTimestamp(),
                        "sequenceNum", sequenceNum).create();
                created++;
            }
            if (topProductIds.size() < maxTopProductIds) {
                topProductIds.add(productId);
            }
            return pcm;
        }

        protected void removeProductCategoryMember(GenericValue pcm) throws GeneralException {
            pcm.remove();
            removed++;
        }
    }
}
