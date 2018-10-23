/**
 * SCIPIO: This script merely invokes another script name dynamically. 
 * In stock can't do this from screens because script loc not context-aware.
 * TODO: support more scripts/args?
 */

import org.ofbiz.base.util.*
import org.ofbiz.base.util.ScriptUtil

if (context.scriptLocation) {
    ScriptUtil.executeScript(context.scriptLocation, context.scriptFunction, context)
}
else {
    Debug.logError("Missing scriptLocation", "runScript.groovy")
}
