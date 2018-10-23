/**
 * SCIPIO: View certificate action code.
 */

import org.ofbiz.base.util.KeyStoreUtil
import org.ofbiz.base.component.ComponentConfig
import org.ofbiz.base.util.Debug

final String module = "ViewCertificate.groovy"

components = ComponentConfig.getAllComponents()
context.components = components ?: []

cert = null
if (parameters.certString) {
    try {
        cert = KeyStoreUtil.pemToCert(parameters.certString)
    }
    catch(Exception e) {
        Debug.logError(e, "Could not get certificate", module)
        errMsg = e.getMessage();
        if (context.errorMessageList == null) {
            context.errorMessageList = []
        }
        context.errorMessageList.add(errMsg)
    }
}
context.cert = cert
