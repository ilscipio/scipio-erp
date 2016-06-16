import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties;

//Debug.log("demo data generator service list");
final DEMO_DATA_GENERATOR_SERVICE_INTERFACE_NAME = "demoDataGenerator";

curDispatchContext = dispatcher.getDispatchContext();
serviceNames = curDispatchContext.getAllServiceNames();

context.maxRecords = UtilProperties.getPropertyAsInteger("general", "data.generator.max.records", 50);

servicesList = new ArrayList();

for (serviceName in serviceNames) {
    curServiceModel = curDispatchContext.getModelService(serviceName);
    if (curServiceModel != null) {
        for (implService in curServiceModel.implServices) {
            if (implService.getService().equals(DEMO_DATA_GENERATOR_SERVICE_INTERFACE_NAME)) {
//                Debug.log("curServiceModel ===========> " + curServiceModel.name);
                curServiceMap = [:];
                engineName = curServiceModel.engineName ?: "NA";
                defaultEntityName = curServiceModel.defaultEntityName ?: "NA";
                invoke = curServiceModel.invoke ?: "NA";
                location = curServiceModel.location ?: "NA";
                requireNewTransaction = curServiceModel.requireNewTransaction;

                curServiceMap.serviceName = serviceName;
                curServiceMap.engineName = engineName;
                curServiceMap.defaultEntityName = defaultEntityName;
                curServiceMap.invoke = invoke;
                curServiceMap.location = location;
                curServiceMap.definitionLocation = curServiceModel.definitionLocation.replaceFirst("file:/" + System.getProperty("ofbiz.home") + "/", "");
                curServiceMap.requireNewTransaction = requireNewTransaction;

                servicesList.add(curServiceMap);
            }
        }
    }
}
context.servicesList = servicesList;