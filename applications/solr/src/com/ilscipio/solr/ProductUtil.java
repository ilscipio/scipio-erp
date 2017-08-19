package com.ilscipio.solr;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;

/**
 * Product utility class for solr.
 */
public abstract class ProductUtil {
    public static final String module = ProductUtil.class.getName();

    public static Map<String, Object> getProductContent(GenericValue product, DispatchContext dctx, Map<String, Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String productId = (String) product.get("productId");
        Map<String, Object> dispatchContext = new HashMap<String, Object>();

        if (Debug.verboseOn()) {
            Debug.logVerbose("Solr: Getting product content for productId '" + productId + "'", module);
        }
        
        try {
            // Generate special ProductContentWrapper for the supported languages (de/en/fr)
            ProductContentWrapper productContentEn = new ProductContentWrapper(dispatcher, product, new Locale("en"), null);
            ProductContentWrapper productContentDe = new ProductContentWrapper(dispatcher, product, new Locale("de"), null);
            ProductContentWrapper productContentFr = new ProductContentWrapper(dispatcher, product, new Locale("fr"), null);
            if (productId != null) {
                dispatchContext.put("productId", productId);
                // if (product.get("sku") != null) dispatchContext.put("sku", product.get("sku"));
                if (product.get("internalName") != null)
                    dispatchContext.put("internalName", product.get("internalName"));
                // GenericValue manu = product.getRelatedOneCache("Manufacturer");
                // if (product.get("manu") != null) dispatchContext.put("manu", "");
                String smallImage = (String) product.get("smallImageUrl");
                if (smallImage != null)
                    dispatchContext.put("smallImage", smallImage);
                String mediumImage = (String) product.get("mediumImageUrl");
                if (mediumImage != null)
                    dispatchContext.put("mediumImage", mediumImage);
                String largeImage = (String) product.get("largeImageUrl");
                if (largeImage != null)
                    dispatchContext.put("largeImage", largeImage);                
                
                // if(product.get("weight") != null) dispatchContext.put("weight", "");

                // Trying to set a correctand trail
                List<GenericValue> category = delegator.findList("ProductCategoryMember", EntityCondition.makeCondition(UtilMisc.toMap("productId", productId)), null, null, null, false);
                List<String> trails = new ArrayList<String>();
                for (Iterator<GenericValue> catIterator = category.iterator(); catIterator.hasNext();) {
                    GenericValue cat = (GenericValue) catIterator.next();
                    String productCategoryId = (String) cat.get("productCategoryId");
                    List<List<String>> trailElements = CategoryUtil.getCategoryTrail(productCategoryId, dctx);
                    for (List<String> trailElement : trailElements) {
                        StringBuilder catMember = new StringBuilder();
                        int i = 0;
                        Iterator<String> trailIter = trailElement.iterator();
                       
                        while (trailIter.hasNext()) {
                            String trailString = (String) trailIter.next();
                            if (catMember.length() > 0){
                                catMember.append("/");
                                i++;
                            }
                            catMember.append(trailString);
                            String cm = i +"/"+ catMember.toString();
                            if (!trails.contains(cm)) {
                                //Debug.logInfo("cm : "+cm, module);
                                trails.add(cm);
                            }
                        }
                        
                    }
                }
                dispatchContext.put("category", trails);

                // Get the catalogs that have associated the categories
                List<String> catalogs = FastList.newInstance();
                for (String trail : trails) {
                    String productCategoryId = (trail.split("/").length > 0) ? trail.split("/")[1] : trail;
                    List<String> catalogMembers = CategoryUtil.getCatalogIdsByCategoryId(delegator, productCategoryId);
                    for (String catalogMember : catalogMembers)
                        if (!catalogs.contains(catalogMember))
                            catalogs.add(catalogMember);
                }
                dispatchContext.put("catalog", catalogs);

                // Alternative
                // if(category.size()>0) dispatchContext.put("category", category);
                // if(product.get("popularity") != null) dispatchContext.put("popularity", "");

                Map<String, Object> featureSet = dispatcher.runSync("getProductFeatureSet", UtilMisc.toMap("productId", productId, "emptyAction", "success"));
                if (featureSet != null) {
                    dispatchContext.put("features", (Set<?>) featureSet.get("featureSet"));
                }

                Map<String, Object> productInventoryAvailable = dispatcher.runSync("getProductInventoryAvailable", UtilMisc.toMap("productId", productId));
                String inStock = null;
                BigDecimal availableToPromiseTotal = (BigDecimal) productInventoryAvailable.get("availableToPromiseTotal");
                if (availableToPromiseTotal != null) {
                    inStock = availableToPromiseTotal.toBigInteger().toString();
                }
                dispatchContext.put("inStock", inStock);

                Boolean isVirtual = ProductWorker.isVirtual(delegator, productId);
                if (isVirtual)
                    dispatchContext.put("isVirtual", isVirtual);
                Boolean isVariant = ProductWorker.isVariant(delegator, productId);
                if (isVariant) // new 2017-08-17
                    dispatchContext.put("isVariant", isVariant); 
                Boolean isDigital = ProductWorker.isDigital(product);
                if (isDigital)
                    dispatchContext.put("isDigital", isDigital);
                Boolean isPhysical = ProductWorker.isPhysical(product);
                if (isPhysical)
                    dispatchContext.put("isPhysical", isPhysical);

                FastMap<String, String> title = new FastMap<String, String>();
                String detitle = productContentDe.get("PRODUCT_NAME");
                if (detitle != null)
                    title.put("de", detitle);
                else if (product.get("productName") != null)
                    title.put("de", (String) product.get("productName"));
                String entitle = productContentEn.get("PRODUCT_NAME");
                if (entitle != null)
                    title.put("en", entitle);
                else if (product.get("productName") != null)
                    title.put("en", (String) product.get("productName"));
                String frtitle = productContentFr.get("PRODUCT_NAME");
                if (frtitle != null)
                    title.put("fr", frtitle);
                else if (product.get("productName") != null)
                    title.put("fr", (String) product.get("productName"));
                dispatchContext.put("title", title);

                Map<String, String> description = new FastMap<String, String>();
                String dedescription = productContentDe.get("DESCRIPTION");
                if (dedescription != null)
                    description.put("de", dedescription);
                String endescription = productContentEn.get("DESCRIPTION");
                if (endescription != null)
                    description.put("en", endescription);
                String frdescription = productContentFr.get("DESCRIPTION");
                if (frdescription != null)
                    description.put("fr", frdescription);
                dispatchContext.put("description", description);

                FastMap<String, String> longDescription = new FastMap<String, String>();
                String delongDescription = productContentDe.get("LONG_DESCRIPTION");
                if (delongDescription != null)
                    longDescription.put("de", delongDescription);
                String enlongDescription = productContentEn.get("LONG_DESCRIPTION");
                if (enlongDescription != null)
                    longDescription.put("en", enlongDescription);
                String frlongDescription = productContentFr.get("LONG_DESCRIPTION");
                if (frlongDescription != null)
                    longDescription.put("fr", frlongDescription);
                dispatchContext.put("longDescription", longDescription);

                // dispatchContext.put("comments", "");
                // dispatchContext.put("keywords", "");
                // dispatchContext.put("last_modified", "");

                if (product != null && "AGGREGATED".equals(product.getString("productTypeId"))) {
                    // FIXME: locale should be looked up differently, but shouldn't have any impacts to price selection...
                    //Locale priceConfigLocale = new Locale("de_DE");
                    Locale priceConfigLocale = (Locale) context.get("locale"); 
                    ProductConfigWrapper configWrapper = new ProductConfigWrapper(delegator, dispatcher, productId, null, null, null, null, priceConfigLocale, userLogin);
                    String listPrice = configWrapper.getTotalListPrice().setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                    if (listPrice != null)
                        dispatchContext.put("listPrice", listPrice);
                    String defaultPrice = configWrapper.getTotalListPrice().setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                    if (defaultPrice != null)
                        dispatchContext.put("defaultPrice", defaultPrice);
                } else {
                    Map<String, GenericValue> priceContext = UtilMisc.toMap("product", product);
                    Map<String, Object> priceMap = dispatcher.runSync("calculateProductPrice", priceContext);
                    if (priceMap.get("listPrice") != null) {
                        String listPrice = ((BigDecimal) priceMap.get("listPrice")).setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                        dispatchContext.put("listPrice", listPrice);
                    }
                    if (priceMap.get("defaultPrice") != null) {
                        String defaultPrice = ((BigDecimal) priceMap.get("defaultPrice")).setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                        if (defaultPrice != null)
                            dispatchContext.put("defaultPrice", defaultPrice);
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, e.getMessage(), module);
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
        }
        return dispatchContext;
    }
}