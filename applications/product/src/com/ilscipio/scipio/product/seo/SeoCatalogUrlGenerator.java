package com.ilscipio.scipio.product.seo;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
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
class SeoCatalogUrlGenerator extends SeoCatalogTraverser {

    protected final Map<String, ?> servCtxOpts;
    
    public SeoCatalogUrlGenerator(DispatchContext dctx, Map<String, ?> servCtxOpts, boolean useCache, boolean doCategory,
            boolean doProduct) throws GeneralException {
        super(dctx.getDelegator(), dctx.getDispatcher(), useCache, doCategory, doProduct);
        this.servCtxOpts = servCtxOpts;
        this.reset();
    }

    @Override
    public void reset() throws GeneralException {
        super.reset();
    }
    
    /**
     * The options used for nested service calls - contains locale and various flag -
     * in some cases can simply be set to the caller's service context (not modified).
     */
    public Map<String, ?> getContext() {
        return servCtxOpts;
    }

    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state)
            throws GeneralException {
        generateCategoryAltUrls(productCategory);
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state)
            throws GeneralException {
        generateProductAltUrls(product, true);
    }

    public void generateCategoryAltUrls(GenericValue productCategory) throws GeneralException {
        String productCategoryId = productCategory.getString("productCategoryId");
        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("generateProductCategoryAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("productCategory", productCategory);
        servCtx.put("productCategoryId", productCategoryId);
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
    
    public void generateProductAltUrls(GenericValue product, boolean doChildProducts) throws GeneralException {
        // NOTE: must check product itself here because generateProductAlternativeUrlsCore will only do it for its children
        boolean includeVariant = Boolean.TRUE.equals(servCtxOpts.get("includeVariant"));
        if (!includeVariant && "Y".equals(product.getString("isVariant"))) {
            return;
        }
        
        String productId = product.getString("productId");
        
        Map<String, Object> servCtx = getDispatcher().getDispatchContext().makeValidContext("generateProductAlternativeUrlsCore", ModelService.IN_PARAM, servCtxOpts);
        servCtx.put("product", product);
        servCtx.put("productId", productId);
        servCtx.put("doChildProducts", doChildProducts);
        servCtx.put("includeVariant", includeVariant);
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