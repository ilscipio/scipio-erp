import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil

context.maxRecords = UtilProperties.getPropertyAsInteger("demosuite", "demosuite.test.data.max.records", 50);

Map<String, Object> dataGeneratorProviders = UtilMisc.getPrefixedMapEntries(UtilProperties.getProperties("demosuite"), "demosuite.test.data.provider.name.");
context.dataGeneratorProviders = dataGeneratorProviders.values();

servicesList = new ArrayList();
servicesList = DemoSuiteDataGeneratorUtil.getDemoDataServices(dispatcher);
context.servicesList = servicesList;

