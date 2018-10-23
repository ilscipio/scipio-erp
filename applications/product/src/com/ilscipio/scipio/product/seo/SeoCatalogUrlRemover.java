package com.ilscipio.scipio.product.seo;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

/**
 * SEO catalog URL category traversal and generation code.
 * <p>
 * NOTE: this currently implements the individual category and product URL generation by delegating to these services:
 * <ul>
 * <li>generateProductCategoryAlternativeUrlsCore</li>
 * <li>generateProductAlternativeUrlsCore</li>
 * </ul>
 */
public class SeoCatalogUrlRemover extends SeoCatalogTraverser {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    static final String logPrefix = "Seo: Alt URLs: ";

    public SeoCatalogUrlRemover(Delegator delegator, LocalDispatcher dispatcher, RemoveTraversalConfig travConfig) throws GeneralException {
        super(delegator, dispatcher, travConfig);
        this.reset();
    }

    public static class RemoveTraversalConfig extends SeoTraversalConfig {
        protected Map<String, ?> servCtxOpts = new HashMap<>();
        protected boolean doChildProducts = true;
        private boolean includeVariant = true;
        private boolean generateFixedIds = true;
        private String prodFixedIdPat = null;
        private String catFixedIdPat = null;

        /**
         * The options used for nested service calls - contains locale, user auth and various flags -
         * in some cases can simply be set to the caller's service context (not modified).
         * NOTE: If this was passed empty to constructor, any errors from sub-services
         * are guaranteed to not be localized.
         */
        public Map<String, ?> getServCtxOpts() {
            return servCtxOpts;
        }

        public RemoveTraversalConfig setServCtxOpts(Map<String, ?> servCtxOpts) {
            this.servCtxOpts = servCtxOpts;
            return this;
        }

        public boolean isDoChildProducts() {
            return doChildProducts;
        }

        public RemoveTraversalConfig setDoChildProducts(boolean doChildProducts) {
            this.doChildProducts = doChildProducts;
            return this;
        }

        public boolean isIncludeVariant() {
            return includeVariant;
        }

        public RemoveTraversalConfig setIncludeVariant(boolean includeVariant) {
            this.includeVariant = includeVariant;
            return this;
        }

        public boolean isGenerateFixedIds() {
            return generateFixedIds;
        }

        public RemoveTraversalConfig setGenerateFixedIds(boolean generateFixedIds) {
            this.generateFixedIds = generateFixedIds;
            return this;
        }

        public String getProdFixedIdPat() {
            return prodFixedIdPat;
        }

        public RemoveTraversalConfig setProdFixedIdPat(String prodFixedIdPat) {
            this.prodFixedIdPat = prodFixedIdPat;
            return this;
        }

        public String getCatFixedIdPat() {
            return catFixedIdPat;
        }

        public RemoveTraversalConfig setCatFixedIdPat(String catFixedIdPat) {
            this.catFixedIdPat = catFixedIdPat;
            return this;
        }
    }

    @Override
    public RemoveTraversalConfig newTravConfig() {
        return new RemoveTraversalConfig();
    }

    @Override
    public RemoveTraversalConfig getTravConfig() {
        return (RemoveTraversalConfig) travConfig;
    }


    @Override
    public void reset() throws GeneralException {
        super.reset();
    }

    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state)
            throws GeneralException {
        removeCategoryAltUrls(productCategory);
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state)
            throws GeneralException {
        removeProductAltUrls(product);
    }

    public void removeCategoryAltUrls(GenericValue productCategory) throws GeneralException {
        Map<String, ?> servCtxOpts = getTravConfig().getServCtxOpts();
        String productCategoryId = productCategory.getString("productCategoryId");
        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("removeProductCategoryAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("productCategory", productCategory);
        servCtx.put("productCategoryId", productCategoryId);
        // service call for separate transaction
        Map<String, Object> recordResult = getDispatcher().runSync("removeProductCategoryAlternativeUrlsCore", servCtx, -1, true);

        if (ServiceUtil.isSuccess(recordResult)) {
            if (Boolean.TRUE.equals(recordResult.get("categoryUpdated"))) {
                getStats().categorySuccess++;
            } else {
                getStats().categorySkipped++;
            }
        } else {
            // caller already logs
            //Debug.logError(getLogMsgPrefix()+"Error removing alternative links for category '"
            //        + productCategoryId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
            getStats().categoryError++;
        }
    }

    public void removeProductAltUrls(GenericValue product) throws GeneralException {
        Map<String, ?> servCtxOpts = getTravConfig().getServCtxOpts();

        // NOTE: must check product itself here because generateProductAlternativeUrlsCore will only do it for its children
        boolean includeVariant = getTravConfig().isIncludeVariant();
        if (!includeVariant && "Y".equals(product.getString("isVariant"))) {
            return;
        }

        String productId = product.getString("productId");

        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("removeProductAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("product", product);
        servCtx.put("productId", productId);
        servCtx.put("doChildProducts", getTravConfig().isDoChildProducts());
        servCtx.put("includeVariant", includeVariant);
        servCtx.put("skipProductIds", getSeenProductIds());
        // service call for separate transaction
        Map<String, Object> recordResult = getDispatcher().runSync("removeProductAlternativeUrlsCore", servCtx, -1, true);

        Integer numUpdated = (Integer) recordResult.get("numUpdated");
        Integer numSkipped = (Integer) recordResult.get("numSkipped");
        Integer numError = (Integer) recordResult.get("numError");
        if (numUpdated != null) getStats().productSuccess += numUpdated;
        if (numSkipped != null) getStats().productSkipped += numSkipped;
        if (ServiceUtil.isSuccess(recordResult)) {
            if (numError != null) getStats().productError += numError;
        } else {
            if (numError != null) getStats().productError += numError;
            else getStats().productError++; // couldn't return count
            // caller already logs
            //Debug.logError(getLogMsgPrefix()+"Error removing alternative links for product '"
            //        + productId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
        }
        Collection<String> visitedProductIds = UtilGenerics.checkCollection(recordResult.get("visitedProductIds"));
        if (visitedProductIds != null) this.registerSeenProductIds(visitedProductIds);
    }

    @Override
    protected String getLogMsgPrefix() {
        return logPrefix;
    }

    @Override
    protected String getLogErrorPrefix() {
        return getLogMsgPrefix()+"Error generating alternative links: ";
    }
}