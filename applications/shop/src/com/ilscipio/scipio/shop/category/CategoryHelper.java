package com.ilscipio.scipio.shop.category;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.service.LocalDispatcher;

import org.ofbiz.base.util.UtilMisc;

public class CategoryHelper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected final HttpServletRequest request;
    protected final Delegator delegator;
    protected final LocalDispatcher dispatcher;
    protected final Locale locale;

    public CategoryHelper(HttpServletRequest request, Delegator delegator, LocalDispatcher dispatcher, Locale locale) {
        super();
        this.request = request;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.locale = locale;
    }
    
    public CategoryHelper(Map<String, Object> context) {
        super();
        this.request = (HttpServletRequest) context.get("request");
        this.delegator = (Delegator) context.get("delegator");
        this.dispatcher = (LocalDispatcher) context.get("dispatcher");
        this.locale = (Locale) context.get("locale");
    }

    public static CategoryHelper newInstance(HttpServletRequest request, Delegator delegator, LocalDispatcher dispatcher, Locale locale) {
        return new CategoryHelper(request, delegator, dispatcher, locale);
    }
    
    public static CategoryHelper newInstance(Map<String, Object> context) {
        return new CategoryHelper(context);
    }
    
    
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> makeCategoryInfos(List<Object> categoryIdsOrItems) {
        List<Map<String, Object>> res = UtilMisc.newList();
        for(Object item : categoryIdsOrItems) {
            if (item != null) {
                if (item instanceof String) {
                    res.add(makeCategoryInfo((String) item));
                }
                else if (item instanceof Map) {
                    res.add(makeCategoryInfo((Map<String, Object>) item));
                }
            }
        }
        return res;       
    }
    
    public Map<String, Object> makeCategoryInfo(Map<String, Object> item) {
        String categoryId = (String) item.get("catId"); // TODO: alternatives
        return makeCategoryInfo(categoryId, item);
    }
    
    public Map<String, Object> makeCategoryInfo(String categoryId) {
        return makeCategoryInfo(categoryId, null);
    }
    
    public Map<String, Object> makeCategoryInfo(String categoryId, Map<String, Object> item) {
        Map<String, Object> info = UtilMisc.newMap();
        info.put("item", item);
        info.put("productCategoryId", categoryId);

        if (UtilValidate.isNotEmpty(categoryId)) {
            info.put("displayName", categoryId); // default
            
            GenericValue productCategory;
            try {
                productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", categoryId).cache().queryOne();
                if (productCategory != null) {
                    String categoryName = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "CATEGORY_NAME", locale, dispatcher, "raw");
                    info.put("categoryName", UtilValidate.isNotEmpty(categoryName) ? categoryName : null);
                    String description = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "DESCRIPTION", locale, dispatcher, "raw");
                    info.put("description", UtilValidate.isNotEmpty(description) ? description : null);
    
                    if (UtilValidate.isNotEmpty(categoryName)) {
                        info.put("displayName", categoryName);
                    }
                    else if (UtilValidate.isNotEmpty(description)) {
                        info.put("displayName", description);
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
        }

        return info;       
    }

}
