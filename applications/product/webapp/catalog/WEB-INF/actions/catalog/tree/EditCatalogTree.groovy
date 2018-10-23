/**
 * SCIPIO: DEFAULT interactive tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditCatalogTree.groovy";

// TODO

// CORE DATA PREP - everything above this can be customized in a duplicated file
GroovyUtil.runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/tree/EditCatalogTreeCore.groovy", null, context);
