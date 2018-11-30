import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.GroovyUtil
import org.ofbiz.entity.GenericValue
import org.ofbiz.service.ModelService

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGeneratorProvider
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders

final String module = "RunDemoDataGenerator";


List<GenericValue> dataGeneratorProviders = delegator.findByAnd("DataGeneratorProvider", ["enabled" : "Y"], ["dataGeneratorProviderName"], false);
List<GenericValue> supportedDataGeneratorProviders = [];

if (parameters.SERVICE_NAME) {
    ModelService curServiceModel = dispatcher.getDispatchContext().getModelService(parameters.SERVICE_NAME);
    // check if the service exist and the engine is java or groovy
    if (curServiceModel) {
        Class<? extends DataGeneratorGroovyBaseScript> clazz = null;
        if (curServiceModel.engineName == "java") {
            clazz = Class.forName(curServiceModel.invoke);
        } else if (curServiceModel.engineName == "groovy") {
            clazz = GroovyUtil.getScriptClassFromLocation(curServiceModel.location);
        } else {
            Debug.logError("Unsupported service engine [" + curServiceModel.engineName + "] for service " + curServiceModel.name, module);
        }
        
        if (clazz) {
            declaredAnnotations = clazz.declaredAnnotations;
            for (annotation in declaredAnnotations) {                
                if (annotation.annotationType() == DataGeneratorProvider) {                    
                    DataGeneratorProviders[] providers = annotation.annotationType().getMethod("providers").invoke(annotation);
                    for (i=0; i < providers.length; i++) {
//                        Debug.log("   engine: " + providers[i].name());
                        for (dataGeneratorProvider in dataGeneratorProviders) {
                            if (dataGeneratorProvider.dataGeneratorProviderId.equals(providers[i].name())) {
//                                Debug.log("supported provider ===> " + dataGeneratorProvider);
                                supportedDataGeneratorProviders.add(dataGeneratorProvider);
                            }
                        }                        
                    }
                }
            }
        }
    }
}

context.dataGeneratorProviders = supportedDataGeneratorProviders;