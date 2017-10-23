import org.ofbiz.base.component.ComponentConfig
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.setup.*;

final module = "SetupAccounting.groovy";

List<String> accountingGLs = [];

Properties scipioSetupProperties = UtilProperties.getProperties("scipiosetup");
accountingComponentProperties = UtilProperties.getPropertyNamesWithPrefixSuffix(scipioSetupProperties, "accounting.component", null, true, true, false);
if (accountingComponentProperties) {
    for (String accountingComponentProperty : accountingComponentProperties) {
        Debug.log("accountingComponentProperty ============> " + accountingComponentProperty);
        accountingComponentName = scipioSetupProperties.getProperty(accountingComponentProperty);
        if (ComponentConfig.componentExists(accountingComponentName)) {
            Debug.log("componentConfig [" + accountingComponentName + "] exists: " + accountingComponentProperty.substring(accountingComponentProperty.lastIndexOf('.') + 1));
            accountingGLs.add(accountingComponentProperty.substring(accountingComponentProperty.lastIndexOf('.') + 1));
        }
    }
}
context.accountingGLs = accountingGLs;
Debug.log("accountingGLs ==============> " + accountingGLs);



//Collection<ComponentConfig> components = ComponentConfig.getAllComponents();
//for (componentConfig : components) {
//    Debug.log("componentConfig: " + componentConfig.)
//}
