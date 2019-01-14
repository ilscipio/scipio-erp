/**
 * SCIPIO: Special orderentry-only script to set the current trail to the current category (best-effort).
 * If it cannot determine one, it clears the trail (so the trail doesn't show for an unrelated category/product).
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;

final module = "SetTrailFromProduct.groovy";

productCategory = context.productCategory;
categoryId = productCategory?.productCategoryId;
if (categoryId) {
    trails = CategoryWorker.getCategoryRollupTrails(delegator, categoryId, [CatalogWorker.getCatalogTopCategoryId(request)], true);
    if (trails) {
        CategoryWorker.setTrail(request, trails[0]);
    } else {
        CategoryWorker.resetTrail(request);
        // NOTE: May happen if someone tries multiple tabs or other cases
        Debug.logWarning("OrderEntry: Could not determine a category trail for category '" + categoryId + "'; setting TOP", module)
    }
}
