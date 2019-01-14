/**
 * SCIPIO: Special orderentry-only script to set the current trail to the current product (best-effort).
 * If it cannot determine one, it clears the trail (so the trail doesn't show for an unrelated product).
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import org.ofbiz.product.product.*;

final module = "SetTrailFromProduct.groovy";

product = context.product;
productId = product?.productId;
if (productId) {
    trails = ProductWorker.getProductRollupTrails(delegator, productId, [CatalogWorker.getCatalogTopCategoryId(request)], true);
    if (trails) {
        CategoryWorker.setTrail(request, trails[0]);
    } else {
        CategoryWorker.resetTrail(request);
        // NOTE: May happen if someone tries multiple tabs or other cases
        Debug.logWarning("OrderEntry: Could not determine a category trail for product '" + productId + "'; setting TOP", module)
    }
}