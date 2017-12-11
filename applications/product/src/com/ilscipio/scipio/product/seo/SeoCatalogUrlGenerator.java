package com.ilscipio.scipio.product.seo;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
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
public class SeoCatalogUrlGenerator extends SeoCatalogTraverser {

    public static final String module = SeoCatalogUrlGenerator.class.getName();
    
    static final String logPrefix = "Seo: Alt URLs: ";
    
    public SeoCatalogUrlGenerator(Delegator delegator, LocalDispatcher dispatcher, GenTraversalConfig travConfig) throws GeneralException {
        super(delegator, dispatcher, travConfig);
        this.reset();
    }

    public static class GenTraversalConfig extends SeoTraversalConfig {
        protected Map<String, ?> servCtxOpts = new HashMap<>();
        protected boolean doChildProducts = true;
        private boolean includeVariant = true;
        private boolean generateFixedIds = true;
        
        /**
         * The options used for nested service calls - contains locale, user auth and various flags -
         * in some cases can simply be set to the caller's service context (not modified).
         * NOTE: If this was passed empty to constructor, any errors from sub-services 
         * are guaranteed to not be localized.
         */
        public Map<String, ?> getServCtxOpts() {
            return servCtxOpts;
        }

        public GenTraversalConfig setServCtxOpts(Map<String, ?> servCtxOpts) {
            this.servCtxOpts = servCtxOpts;
            this.includeVariant = !Boolean.FALSE.equals(servCtxOpts.get("includeVariant"));
            return this;
        }

        public boolean isDoChildProducts() {
            return doChildProducts;
        }

        public GenTraversalConfig setDoChildProducts(boolean doChildProducts) {
            this.doChildProducts = doChildProducts;
            return this;
        }
        
        public boolean isIncludeVariant() {
            return includeVariant;
        }

        public boolean isGenerateFixedIds() {
            return generateFixedIds;
        }

        public GenTraversalConfig setGenerateFixedIds(boolean generateFixedIds) {
            this.generateFixedIds = generateFixedIds;
            return this;
        }

        public void setIncludeVariant(boolean includeVariant) {
            this.includeVariant = includeVariant;
        }
    }
    
    @Override
    public GenTraversalConfig newTravConfig() {
        return new GenTraversalConfig();
    }

    @Override
    public GenTraversalConfig getTravConfig() {
        return (GenTraversalConfig) travConfig;
    }
    
    
    @Override
    public void reset() throws GeneralException {
        super.reset();
    }

    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state)
            throws GeneralException {
        generateCategoryAltUrls(productCategory);
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state)
            throws GeneralException {
        generateProductAltUrls(product);
    }

    public void generateCategoryAltUrls(GenericValue productCategory) throws GeneralException {
        Map<String, ?> servCtxOpts = getTravConfig().getServCtxOpts();
        String productCategoryId = productCategory.getString("productCategoryId");
        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("generateProductCategoryAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("productCategory", productCategory);
        servCtx.put("productCategoryId", productCategoryId);
        servCtx.put("generateFixedIds", getTravConfig().isGenerateFixedIds());
        // service call for separate transaction
        Map<String, Object> recordResult = getDispatcher().runSync("generateProductCategoryAlternativeUrlsCore", servCtx, -1, true);
        
        if (ServiceUtil.isSuccess(recordResult)) {
            if (Boolean.TRUE.equals(recordResult.get("categoryUpdated"))) {
                getStats().categorySuccess++;
            } else {
                getStats().categorySkipped++;
            }
        } else {
            Debug.logError(getLogMsgPrefix()+"Error generating alternative links for category '" 
                    + productCategoryId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
            getStats().categoryError++;
        }
    }
    
    public void generateProductAltUrls(GenericValue product) throws GeneralException {
        Map<String, ?> servCtxOpts = getTravConfig().getServCtxOpts();

        // NOTE: must check product itself here because generateProductAlternativeUrlsCore will only do it for its children
        boolean includeVariant = getTravConfig().isIncludeVariant();
        if (!includeVariant && "Y".equals(product.getString("isVariant"))) {
            return;
        }
        
        String productId = product.getString("productId");
        
        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("generateProductAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("product", product);
        servCtx.put("productId", productId);
        servCtx.put("doChildProducts", getTravConfig().isDoChildProducts());
        servCtx.put("includeVariant", includeVariant);
        servCtx.put("generateFixedIds", getTravConfig().isGenerateFixedIds());
        // service call for separate transaction
        Map<String, Object> recordResult = getDispatcher().runSync("generateProductAlternativeUrlsCore", servCtx, -1, true);

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
            Debug.logError(getLogMsgPrefix()+"Error generating alternative links for product '" 
                    + productId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
        }
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