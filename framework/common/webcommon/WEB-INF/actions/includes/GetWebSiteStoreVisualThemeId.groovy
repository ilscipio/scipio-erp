/**
 * SCIPIO: Returns the visualThemeId for the site or product store for the site, best-effort.
 * <p>
 * For use with WebSite.visualThemeSelectorScript entity field.
 * <p>
 * Input:
 *  context.webSite (GenericValue)
 * Output:
 *  context.visualThemeId
 * <p>
 * TODO: does not take into account user login, which could potentially select among
 * many themes within a store.
 */

import javax.servlet.http.HttpSession

import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.store.ProductStoreWorker;
 
visualThemeId = null;

if (context.webSite != null && context.webSite.visualThemeId) {
    // 2016-08-09: It's now possible to override a visualThemeId on the WebSite
    // which takes precedence over the productStore one.
    // The downside is it could not work in a context where WebSite cannot be determined.
    visualThemeId = context.webSite.visualThemeId;
    
    // TODO: for ultimate versatility, we'd want a WebSiteProductStoreSettings which
    // contains a visualThemeId to combine WebSite and productStoreId. In that case,
    // the more specific visualThemeId would override the one above.
} else if (context.request != null) {
    // Is a web request. Simply get visualThemeId from associated store for now.
    productStoreId = null;
    
    // FIXME? not sure if should allow session check here... but this is what ecommerce did in stock
    // via org.ofbiz.product.store.ProductStoreWorker.getProductStore(ServletRequest)
    HttpSession session = context.request.getSession(false);
    if (session != null && session.getAttribute("productStoreId") != null) {
        productStoreId = (String) session.getAttribute("productStoreId");
    }
    
    if (!productStoreId && context.webSite != null) {
        productStoreId = webSite.getString("productStoreId");
    }
    
    if (productStoreId) {
        productStore = ProductStoreWorker.getProductStore(productStoreId, context.delegator);
        if (productStore != null) {
            visualThemeId = productStore.getString("visualThemeId");
        }
    }
}
else {
    // Service call. Probably email.
    // NOTE: in this case, "parameters" map usually contains the email bodyParameters.
    // This is more complicated, because store could come from OrderHeader or other, so check that first,
    // but should have webSite to fall back on (was needed to invoke this script)

    productStoreId = null;
    
    // heuristic to check if this is an order-related email
    if (context.parameters?.baseUrl && context.parameters?.orderId) {
        orderHeader = EntityQuery.use(context.delegator).from("OrderHeader").where("orderId", context.parameters.orderId).cache().queryOne();
        if (orderHeader != null) {
            productStoreId = orderHeader.getString("productStoreId");
        }
    }
    
    if (!productStoreId && (context.webSite != null)) {
        productStoreId = webSite.getString("productStoreId");
    }
    
    if (productStoreId) {
        productStore = ProductStoreWorker.getProductStore(productStoreId, context.delegator);
        if (productStore != null) {
            visualThemeId = productStore.getString("visualThemeId");
        }
    }
}

context.visualThemeId = visualThemeId ?: null;



