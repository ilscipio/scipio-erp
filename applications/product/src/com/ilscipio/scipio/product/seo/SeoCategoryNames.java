package com.ilscipio.scipio.product.seo;

import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.util.SeoStringUtil;

/**
 * SCIPIO
 * @deprecated TODO: REVIEW: category names map - may not be here long.
 * FIXME: remove HttpServletRequest, content wrapper, most of this...
 */
@Deprecated
public class SeoCategoryNames {
    
    private static final SeoCategoryNames DEFAULT_INSTANCE = new SeoCategoryNames();
    
    private Map<String, String> nameIdMap = new HashMap<>();
    private Map<String, String> idNameMap = new HashMap<>();
    private boolean initialized = false;

    public static SeoCategoryNames getDefaultInstance() { 
        return DEFAULT_INSTANCE;
    }
    
    public boolean isInitialized() {
        return initialized;
    }
    
    @Deprecated
    public Map<String, String> getNameIdMap() {
        return nameIdMap;
    }
    @Deprecated
    public Map<String, String> getIdNameMap() {
        return idNameMap;
    }
    
    /**
     * Initial category-name/category-id map.
     * Note: as a key, the category-name should be:
     *         1. ascii
     *         2. lower cased and use hyphen between the words.
     *       If not, the category id will be used.
     * 
     */
    public void init(HttpServletRequest request) {
        if (isInitialized() || !SeoConfigUtil.isCategoryUrlEnabledStatic()) return;
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        initInternal(request, delegator);
    }
    
    private synchronized void initInternal(HttpServletRequest request, Delegator delegator) {
        if (isInitialized()) return; // NOTE: double check required
        
        Map<String, String> nameIdMap = new HashMap<>();
        Map<String, String> idNameMap = new HashMap<>();

        try {
            Collection<GenericValue> allCategories = delegator.findList("ProductCategory", null, UtilMisc.toSet("productCategoryId", "categoryName"), null, null, false);
            for (GenericValue category : allCategories) {
                //String categoryName = category.getString("categoryName");
                String categoryName = getCategoryName(request, category);
                String categoryNameId = null;
                String categoryIdName = null;
                String categoryId = category.getString("productCategoryId");
                if (UtilValidate.isNotEmpty(categoryName)) {
                    categoryName = SeoUrlUtil.replaceSpecialCharsUrl(categoryName.trim(), SeoConfigUtil.getCharFilters());
                    categoryName = SeoConfigUtil.limitCategoryNameLength(categoryName);
                    if (SeoStringUtil.ASCII_PATTERN.matcher(categoryName).matches()) {
                        categoryIdName = categoryName.replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                        // SCIPIO: Note: The hyphen + ID is necessary because of possible duplicate category names!
                        categoryNameId = categoryIdName + SeoStringUtil.URL_HYPHEN + categoryId.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                    } else {
                        categoryIdName = categoryId.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                        categoryNameId = categoryIdName;
                    }
                } else {
                    GenericValue productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", categoryId), true);
                    CategoryContentWrapper wrapper = new CategoryContentWrapper(productCategory, request);
                    String alternativeUrl = wrapper.get("ALTERNATIVE_URL");
                    if (UtilValidate.isNotEmpty(alternativeUrl)) {
                        categoryIdName = SeoUrlUtil.replaceSpecialCharsUrl(alternativeUrl, SeoConfigUtil.getCharFilters());
                        categoryIdName = SeoConfigUtil.limitCategoryNameLength(categoryIdName);
                        // SCIPIO: Note: The hyphen + ID is necessary because of possible duplicate category names!
                        categoryNameId = categoryIdName + SeoStringUtil.URL_HYPHEN + categoryId.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                    } else {
                        categoryNameId = categoryId.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                        categoryIdName = categoryNameId;
                    }
                }
                if (nameIdMap.containsKey(categoryNameId)) {
                    categoryNameId = categoryId.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
                    categoryIdName = categoryNameId;
                }
                if (!SeoStringUtil.ASCII_PATTERN.matcher(categoryNameId).matches() || nameIdMap.containsKey(categoryNameId)) {
                    continue;
                }
                nameIdMap.put(categoryNameId, categoryId);
                nameIdMap.put("Allgemein" + SeoStringUtil.URL_HYPHEN + categoryId, categoryId);
                idNameMap.put(categoryId, categoryIdName);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, SeoCatalogUrlWorker.module);
        }
        
        this.nameIdMap = nameIdMap;
        this.idNameMap = idNameMap;
        
        initialized = true;
    }

    /**
     * SCIPIO: another patch because SEO patch doesn't check Content for categ name.
     */
    @Deprecated
    public String getCategoryName(HttpServletRequest request, GenericValue productCategory) {
        Locale locale;
        locale = SeoConfigUtil.getNamesLocaleOverride();
        if (locale == null) {
            locale = UtilHttp.getLocale(request);
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "CATEGORY_NAME", locale, dispatcher, "raw");
    }
    
}