/**
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditGlTree.groovy";

//// FIXME?: setupEctMaxProductsPerCat is a session-based control for the time being, breaking convention with rest of setup
//ectMaxProductsPerCat = context.ectMaxProductsPerCat;
//if (ectMaxProductsPerCat == null) {
//    try {
//        ectMaxProductsPerCat = (request.getAttribute("setupEctMaxProductsPerCat") ?: request.getParameter("setupEctMaxProductsPerCat")) as Integer;
//    } catch(Exception e) {
//    }
//    if (ectMaxProductsPerCat != null) {
//        session.setAttribute("setupEctMaxProductsPerCat", ectMaxProductsPerCat);
//    }
//}
//if (ectMaxProductsPerCat == null) ectMaxProductsPerCat = 0; // DEFAULT ZERO: fastest and least confusing
//context.ectMaxProductsPerCat = ectMaxProductsPerCat;
//
//context.ectEventStates = context.eventStates;
//
//// CORE DATA PREP
//GroovyUtil.runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/tree/EditCatalogTreeCore.groovy", null, context);
