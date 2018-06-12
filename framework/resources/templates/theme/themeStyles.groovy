/**
 * Master HTML templating variables include for @component-resource-name@ theme.
 *
 * Overrides the default SCIPIO styles located in
 * htmlVariables.groovy - ofbiz_foundation/framework/common/webcommon/includes/scipio/lib/standard/
 *
 */

import java.lang.*;
import java.util.*;
import org.ofbiz.base.util.*;
 
/*
*************************************
* MAIN STYLES *
*************************************
*/

/* EXTENDS/MODIFIES the default foundation styles */
GroovyUtil.runScriptAtLocation("component://common/webcommon/includes/scipio/lib/standard/htmlVariables.groovy", null, context);

context.styles.putAll([
    /* Always declare last */
    "dummy" : ""
]);
