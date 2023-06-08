package org.ofbiz.product.store;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

/**
 * ProductStoreServices.
 * <p>
 * SCIPIO: Added 2018-09-26.
 */
public class ProductStoreServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "ProductUiLabels";

    public static Map<String, Object> setProductStoreDefaultWebSite(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String productStoreId = (String) context.get("productStoreId");
        String webSiteId = (String) context.get("webSiteId");
        
        List<GenericValue> webSiteList;
        try {
            webSiteList = EntityQuery.use(delegator).from("WebSite").where("productStoreId", productStoreId).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        boolean webSiteFound = false;
        for(GenericValue webSite : webSiteList) {
            if (webSite.getString("webSiteId").equals(webSiteId)) {
                webSiteFound = true;
                if (!Boolean.TRUE.equals(webSite.getBoolean("isStoreDefault"))) {
                    webSite.set("isStoreDefault", "Y");
                }
            } else {
                if (Boolean.TRUE.equals(webSite.getBoolean("isStoreDefault"))) {
                    webSite.set("isStoreDefault", null);
                }
            }
        }
        if (!webSiteFound) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ProductWebSiteNotFoundForStore", 
                    UtilMisc.toMap("webSiteId", webSiteId, "productStoreId", productStoreId), (Locale) context.get("locale") ));
        }
        try {
            delegator.storeAll(webSiteList);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        return ServiceUtil.returnSuccess();
    }

    public static class SetProductStoreLocaleStringsToDefault extends LocalService {
        protected Collection<String> productStoreIds;

        @Override
        public void init(ServiceContext ctx) throws GeneralException {
            super.init(ctx);
            productStoreIds = ctx.attr("productStoreIdList");
            if (UtilValidate.isEmpty(productStoreIds)) {
                String productStoreId = ctx.attr("productStoreId");
                if (UtilValidate.isNotEmpty(productStoreId)) {
                    productStoreIds = List.of(productStoreId);
                } else {
                    throw new ServiceValidationException("Missing productStoreId(s)", ctx.service());
                }
            }
        }

        @Override
        public Map<String, Object> exec() throws GeneralException {
            for (String productStoreId : productStoreIds) {
                GenericValue productStore = ctx.delegator().from("ProductStore").where("productStoreId", productStoreId).queryOne();
                if (productStore == null) {
                    return ctx.error("ProductStore [" + productStoreId + "] not found");
                }
                String defaultLocaleString = productStore.getString("defaultLocaleString");
                if (defaultLocaleString == null) {
                    return ctx.error("ProductStore [" + productStoreId + "] missing defaultLocaleString");
                }
                productStore.setJson("localeStrings", List.of(defaultLocaleString));
                productStore.store();
            }
            return ctx.success("Updated ProductStore localeStrings for: " + productStoreIds);
        }
    }

}
