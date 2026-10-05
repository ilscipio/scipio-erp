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
package org.ofbiz.service.test;

import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * Tests for @Service annotation-based service definitions.
 *
 * <p>SCIPIO: 4.0.0: Added for service annotations testing.</p>
 */
public class ServiceAnnotationTests extends OFBizTestCase {

    public ServiceAnnotationTests(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
    }

    @Override
    protected void tearDown() throws Exception {
    }

    /**
     * Test simple static method service with @Service annotation.
     * Tests serviceAnnotationsTest1 from ServiceTestServices.java
     */
    public void testStaticMethodService() throws Exception {
        Map<String, Object> result = dispatcher.runSync("serviceAnnotationsTest1",
            UtilMisc.toMap("param1", "test input value"));
        assertEquals("Service result success", ModelService.RESPOND_SUCCESS, result.get(ModelService.RESPONSE_MESSAGE));
        assertNotNull("Result1 should be returned", result.get("result1"));
        assertEquals("Result1 value", "test result value 1", result.get("result1"));
    }

    /**
     * Test LocalService class-based service with @Service annotation.
     * Tests ServiceAnnotationsTest2 from ServiceTestServices.java
     */
    public void testLocalServiceClass() throws Exception {
        Map<String, Object> result = dispatcher.runSync("ServiceAnnotationsTest2",
            UtilMisc.toMap("param1", "value1", "param2", "value2"));
        assertEquals("Service result success", ModelService.RESPOND_SUCCESS, result.get(ModelService.RESPONSE_MESSAGE));
        assertNotNull("Result1 should be returned", result.get("result1"));
        // Result should contain concatenated params
        String result1 = (String) result.get("result1");
        assertTrue("Result1 should contain param values", result1.contains("value1") && result1.contains("value2"));
    }

    /**
     * Test service inheritance with @Implements annotation.
     * Tests ServiceAnnotationsTest3 which extends ServiceAnnotationsTest2
     */
    public void testServiceInheritance() throws Exception {
        Map<String, Object> result = dispatcher.runSync("ServiceAnnotationsTest3",
            UtilMisc.toMap("param1", "v1", "param2", "v2", "param3", "v3"));
        assertEquals("Service result success", ModelService.RESPOND_SUCCESS, result.get(ModelService.RESPONSE_MESSAGE));
        assertNotNull("Result1 should be returned", result.get("result1"));
        // Result should contain all three param values
        String result1 = (String) result.get("result1");
        assertTrue("Result1 should contain all param values",
            result1.contains("v1") && result1.contains("v2") && result1.contains("v3"));
    }

    /**
     * Test complex service with entity attributes and multiple features.
     * Tests serviceAnnotationsTest4Extended from ServiceTestServices.java
     */
    public void testComplexServiceWithEntityAttributes() throws Exception {
        // This service has defaultEntityName="Person" with auto-attributes
        Map<String, Object> result = dispatcher.runSync("serviceAnnotationsTest4Extended",
            UtilMisc.toMap("param1", "test1", "param2", "test2", "param3", "test3", "param4", "test4"));
        assertEquals("Service result success", ModelService.RESPOND_SUCCESS, result.get(ModelService.RESPONSE_MESSAGE));
        assertNotNull("Result1 should be returned", result.get("result1"));
        assertNotNull("Result2 should be returned", result.get("result2"));
    }

    /**
     * Test service with SECA and EECA annotations.
     * Tests ServiceAnnotationsTest4c from ServiceTestServices.java
     */
    public void testServiceWithSecaEeca() throws Exception {
        Map<String, Object> result = dispatcher.runSync("ServiceAnnotationsTest4c",
            UtilMisc.toMap("stringParam1", "seca-test", "stringParam1b", "eeca-test"));
        assertEquals("Service result success", ModelService.RESPOND_SUCCESS, result.get(ModelService.RESPONSE_MESSAGE));
    }

    /**
     * Test that annotated service is properly registered in the service engine.
     */
    public void testServiceRegistration() throws Exception {
        // Verify service can be found by name
        ModelService modelService = dispatcher.getDispatchContext().getModelService("serviceAnnotationsTest1");
        assertNotNull("serviceAnnotationsTest1 should be registered", modelService);

        modelService = dispatcher.getDispatchContext().getModelService("ServiceAnnotationsTest2");
        assertNotNull("ServiceAnnotationsTest2 should be registered", modelService);

        modelService = dispatcher.getDispatchContext().getModelService("serviceAnnotationsTest4Extended");
        assertNotNull("serviceAnnotationsTest4Extended should be registered", modelService);
    }
}
