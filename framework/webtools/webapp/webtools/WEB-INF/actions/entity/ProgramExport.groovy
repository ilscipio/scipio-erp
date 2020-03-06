/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.base.util.*
import org.w3c.dom.Document;

import org.codehaus.groovy.control.customizers.ImportCustomizer;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.codehaus.groovy.control.MultipleCompilationErrorsException;
import org.codehaus.groovy.control.ErrorCollector;

String groovyProgram = null;
recordValues = [];
errMsgList = [];

if (UtilValidate.isEmpty(parameters.groovyProgram)) {
    
    groovyProgram = '''
// Use the List variable recordValues to fill it with GenericValue maps.
// Full groovy script syntax is available.

// Example: Find the first three record in the product entity (if any)
products = from("Product").maxRows(3).queryList();
recordValues.addAll(products);


'''
    parameters.groovyProgram = groovyProgram;
} else {
    groovyProgram = parameters.groovyProgram;
}

// SCIPIO: 
langVariant = GroovyUtil.GroovyLangVariant.STANDARD;

// Add imports for script.
def importCustomizer = new ImportCustomizer()
importCustomizer.addImport("org.ofbiz.entity.GenericValue");
importCustomizer.addImport("org.ofbiz.entity.model.ModelEntity");
importCustomizer.addStarImports("org.ofbiz.entity.condition.", "org.ofbiz.entity.util."); // SCIPIO
def configuration = new CompilerConfiguration(langVariant.getGroovyCompilerConfiguration()) // SCIPIO: derive from STANDARD 
configuration.addCompilationCustomizers(importCustomizer)

// SCIPIO: This is the bare minimum to get better context var availability here
//Binding binding = new Binding();
Binding binding = langVariant.createBinding(context);
binding.setVariable("delegator", delegator);
binding.setVariable("recordValues", recordValues);

// SCIPIO
//ClassLoader loader = Thread.currentThread().getContextClassLoader();
ClassLoader loader = langVariant.createGroovyClassLoader();
def shell = new GroovyShell(loader, binding, configuration);

if (UtilValidate.isNotEmpty(groovyProgram) && 
    context.hasTmplTestPerm == true && request.getMethod().toLowerCase() == "post") { // SCIPIO: hasTmplTestPerm, POST
    try {
        shell.parse(groovyProgram);
        shell.evaluate(groovyProgram)
        recordValues = shell.getVariable("recordValues");
        xmlDoc = GenericValue.makeXmlDocument(recordValues);
        context.put("xmlDoc", xmlDoc);
    } catch(MultipleCompilationErrorsException e) {
        request.setAttribute("_ERROR_MESSAGE_", e);
        return;
    } catch(groovy.lang.MissingPropertyException e) {
        request.setAttribute("_ERROR_MESSAGE_", e);
        return;
    } catch(IllegalArgumentException e) {
        request.setAttribute("_ERROR_MESSAGE_", e);
        return;
    } catch(NullPointerException e) {
        request.setAttribute("_ERROR_MESSAGE_", e);
        return;
    } catch(Exception e) {
        request.setAttribute("_ERROR_MESSAGE_", e);
        return;
    } 
}
