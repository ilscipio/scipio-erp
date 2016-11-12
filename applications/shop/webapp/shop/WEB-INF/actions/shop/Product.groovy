import java.lang.*;
import java.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.entity.util.EntityUtil;

final module = "ProductGroovy";

// SCIPIO: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) {
    localVarsOnly = false;
}
context.remove("localVarsOnly");
// SCIPIO: In some screens may need to read request vars/params, but not update request...
updateRequestVars = context.updateRequestVars;
if (updateRequestVars == null) {
    if (localVarsOnly) {
        updateRequestVars = false;
    } else {
        updateRequestVars = true; // default true for now (legacy)
    }
}
context.remove("updateRequestVars");

requestParams = UtilHttp.getParameterMap(request);

productId = requestParams.product_id ?: request.getAttribute("product_id");
productCategoryId = request.getAttribute("productCategoryId");

if (productId) {
    if (updateRequestVars) {
        // SCIPIO: If we're allowed to update request vars, do this call which will ensure
        // the session trail and request attributes are set in the case that it was not already done previously in request.
        // In stock Ofbiz, this was only done in special filter requests, but this leaves other requests barren.
        // NOTE: This could also be done in uri="product" request as event, but here it has more chances of running
        productCategoryId = org.ofbiz.product.category.CatalogUrlFilter.getAdjustCurrentCategoryAndProduct(request, productId, productCategoryId);
    }
}

GroovyUtil.runScriptAtLocation("component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/Product.groovy", null, context);


