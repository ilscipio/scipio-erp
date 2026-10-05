/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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