import java.util.List
import java.util.Map

import org.ofbiz.base.util.UtilProperties;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil

context.maxRecords = UtilProperties.getPropertyAsInteger("demosuite", "demosuite.test.data.max.records", 50);

List<Map<String, Object>> servicesList = DemoSuiteDataGeneratorUtil.getDemoDataServices(dispatcher);
context.servicesList = servicesList;