import org.ofbiz.base.component.ComponentConfig
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.setup.*;

final module = "SetupAccounting.groovy";

List<ComponentConfig> accountingCompoments = [];
availableAccountingComponents = UtilProperties.getPropertyValue("scipiosetup", "accounting.components.available");
if (availableAccountingComponents) {
    String[] availableAccountingComponentNames = availableAccountingComponents.split(",");
    for (String accountingComponentName : availableAccountingComponentNames) {
       if (ComponentConfig.componentExists(accountingComponentName)) {
           Debug.log("componentConfig [" + accountingComponentName + "] exists");
           accountingCompoments.add(ComponentConfig.getComponentConfig(accountingComponentName));
       }
    }
}
context.accountingCompoments = accountingCompoments;

//Collection<ComponentConfig> components = ComponentConfig.getAllComponents();
//for (componentConfig : components) {
//    Debug.log("componentConfig: " + componentConfig.)
//}
