import org.ofbiz.entity.GenericValue

List<GenericValue> dataGeneratorProviders = delegator.findByAnd("DataGeneratorProvider", ["enabled" : "Y"], ["dataGeneratorProviderName"], false);
context.dataGeneratorProviders = dataGeneratorProviders;