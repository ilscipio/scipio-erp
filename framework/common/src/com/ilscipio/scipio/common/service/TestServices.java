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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TestServices {

    /**
     * Test service
     */
    @Service(
        name = "testScv",
        location = "org.ofbiz.common.CommonServices",
        invoke = "testService",
        description = "Test service",
        export = "true",
        validate = "false",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "defaultValue", type = "Double", mode = "IN", defaultValue = "999.9999"),
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface TestScv {}

    /**
     * Test SOAP service
     */
    @Service(
        name = "testSOAPScv",
        location = "org.ofbiz.common.CommonServices",
        invoke = "testSOAPService",
        description = "Test SOAP service",
        export = "true",
        validate = "false",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "testing", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "testingNodes", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface TestSOAPScv {}

    /**
     * Blocking Test service
     */
    @Service(
        name = "blockingTestScv",
        location = "org.ofbiz.common.CommonServices",
        invoke = "blockingTestService",
        description = "Blocking Test service",
        validate = "false",
        requireNewTransaction = "true",
        transactionTimeout = "20",
        attributes = {
            @Attribute(name = "duration", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface BlockingTestScv {}

    @Service(
        name = "testError",
        location = "org.ofbiz.common.CommonServices",
        invoke = "returnErrorService",
        export = "true",
        validate = "false",
        requireNewTransaction = "true",
        maxRetry = "1"
    )
    public interface TestError {}

    /**
     * Test service
     */
    @Service(
        name = "testRollback",
        location = "org.ofbiz.common.CommonServices",
        invoke = "testRollbackListener",
        description = "Test service",
        export = "true",
        validate = "false",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface TestRollback {}

    /**
     * Test service
     */
    @Service(
        name = "testCommit",
        location = "org.ofbiz.common.CommonServices",
        invoke = "testCommitListener",
        description = "Test service",
        export = "true",
        validate = "false",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface TestCommit {}

    @Service(
        name = "groupTest",
        engine = "group",
        location = "testGroup"
    )
    public interface GroupTest {}

    /**
     * HTTP service wrapper around the test service
     */
    @Service(
        name = "testHttp",
        engine = "http",
        location = "main-http",
        invoke = "testScv",
        description = "HTTP service wrapper around the test service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface TestHttp {}

    /**
     * SOAP service; calls the OFBiz test SOAP service
     */
    @Service(
        name = "testSoap",
        engine = "soap",
        location = "main-soap",
        invoke = "testSOAPScv",
        description = "SOAP service; calls the OFBiz test SOAP service",
        export = "true",
        implemented = {@Implements(service = "testSOAPScv")}
    )
    public interface TestSoap {}

    /**
     * simple SOAP service; calls the OFBiz test service
     */
    @Service(
        name = "testSoapSimple",
        engine = "soap",
        location = "main-soap",
        invoke = "testScv",
        description = "simple SOAP service; calls the OFBiz test service",
        export = "true",
        implemented = {@Implements(service = "testScv")}
    )
    public interface TestSoapSimple {}

    @Service(
        name = "testRemoteSoap",
        engine = "soap",
        location = "https://ce.scipioerp.com/admin/control/SOAPService",
        invoke = "testSoapSimple",
        export = "true",
        attributes = {
            @Attribute(name = "defaultValue", type = "Double", mode = "IN", defaultValue = "999.9999"),
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "resp", type = "String", mode = "OUT")
        }
    )
    public interface TestRemoteSoap {}

    /**
     * A service to invoke the NWS web service
     */
    @Service(
        name = "testRemoteSoap1",
        engine = "soap",
        location = "https://graphical.weather.gov/xml/SOAP_server/ndfdXMLserver.php",
        invoke = "LatLonListZipCode",
        description = "A service to invoke the NWS web service",
        export = "true",
        attributes = {
            @Attribute(name = "zipCodeList", type = "String", mode = "IN"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestRemoteSoap1 {}

    /**
     * A service to invoke the NWS web service
     */
    @Service(
        name = "testRemoteSoap2",
        engine = "soap",
        location = "http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php",
        invoke = "LatLonListCityNames",
        description = "A service to invoke the NWS web service",
        export = "true",
        attributes = {
            @Attribute(name = "CityName", type = "String", mode = "IN"),
            @Attribute(name = "invoke", type = "String", mode = "IN"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestRemoteSoap2 {}

    @Service(
        name = "testRemoteSoap3",
        engine = "soap",
        location = "http://www.restfulwebservices.net/wcf/EmailValidationService.svc",
        invoke = "EmailValidationService",
        export = "true",
        attributes = {
            @Attribute(name = "ZipCode", type = "String", mode = "IN"),
            @Attribute(name = "invoke", type = "String", mode = "IN"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestRemoteSoap3 {}

    @Service(
        name = "testRemoteSoap4",
        engine = "soap",
        location = "http://www.webservicex.net/geoipservice.asmx",
        invoke = "GetGeoIPContext",
        export = "true",
        attributes = {
            @Attribute(name = "invoke", type = "String", mode = "IN"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestRemoteSoap4 {}

    /**
     * Test BeanShell Script Service
     */
    @Service(
        name = "testBsh",
        engine = "bsh",
        location = "component://common/script/org/ofbiz/common/BshServiceTest.bsh",
        description = "Test BeanShell Script Service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestBsh {}

    /**
     * Test Groovy Script Service
     */
    @Service(
        name = "testGroovy",
        engine = "groovy",
        location = "component://common/script/org/ofbiz/common/GroovyServiceTest.groovy",
        description = "Test Groovy Script Service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestGroovy {}

    /**
     * Test Groovy Script Service Method Invocation
     */
    @Service(
        name = "testGroovyMethod",
        engine = "groovy",
        location = "component://common/script/org/ofbiz/common/GroovyServiceTest.groovy",
        invoke = "testMethod",
        description = "Test Groovy Script Service Method Invocation",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestGroovyMethod {}

    /**
     * Test BeanShell Script Service
     */
    @Service(
        name = "testScriptEngineBsh",
        engine = "script",
        location = "component://common/script/org/ofbiz/common/BshServiceTest.bsh",
        description = "Test BeanShell Script Service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestScriptEngineBsh {}

    /**
     * Test Script Engine With Groovy Script
     */
    @Service(
        name = "testScriptEngineGroovy",
        engine = "script",
        location = "component://common/script/org/ofbiz/common/GroovyServiceTest.groovy",
        description = "Test Script Engine With Groovy Script",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestScriptEngineGroovy {}

    /**
     * Test Script Engine With Groovy Script Method Invocation
     */
    @Service(
        name = "testScriptEngineGroovyMethod",
        engine = "script",
        location = "component://common/script/org/ofbiz/common/GroovyServiceTest.groovy",
        invoke = "testMethod",
        description = "Test Script Engine With Groovy Script Method Invocation",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestScriptEngineGroovyMethod {}

    /**
     * Test Script Engine With JavaScript
     */
    @Service(
        name = "testScriptEngineJavaScript",
        engine = "script",
        location = "component://common/script/org/ofbiz/common/JavaScriptTest.js",
        description = "Test Script Engine With JavaScript",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "exampleId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestScriptEngineJavaScript {}

    /**
     * Test Script Engine With JavaScript Function Invocation
     */
    @Service(
        name = "testScriptEngineJavaScriptFunction",
        engine = "script",
        location = "component://common/script/org/ofbiz/common/JavaScriptTest.js",
        invoke = "testFunction",
        description = "Test Script Engine With JavaScript Function Invocation",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT")
        }
    )
    public interface TestScriptEngineJavaScriptFunction {}

    /**
     * Test JMS Queue service
     */
    @Service(
        name = "testJMSQueue",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "testScv",
        description = "Test JMS Queue service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN")
        }
    )
    public interface TestJMSQueue {}

    /**
     * Test JMS Topic service
     */
    @Service(
        name = "testJMSTopic",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "testScv",
        description = "Test JMS Topic service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN")
        }
    )
    public interface TestJMSTopic {}

    /**
     * Test Service MCA
     */
    @Service(
        name = "testMca",
        location = "org.ofbiz.common.CommonServices",
        invoke = "mcaTest",
        description = "Test Service MCA",
        implemented = {@Implements(service = "mailProcessInterface")}
    )
    public interface TestMca {}

    /**
     * Test the Route engine
     */
    @Service(
        name = "testRoute",
        engine = "route",
        description = "Test the Route engine",
        auth = "true"
    )
    public interface TestRoute {}

    /**
     * To test XML-RPC handling of Maps and Lists
     */
    @Service(
        name = "simpleMapListTest",
        location = "org.ofbiz.common.CommonServices",
        invoke = "simpleMapListTest",
        description = "To test XML-RPC handling of Maps and Lists",
        export = "true",
        attributes = {
            @Attribute(name = "listOfStrings", type = "List", mode = "IN"),
            @Attribute(name = "mapOfStrings", type = "Map", mode = "IN")
        }
    )
    public interface SimpleMapListTest {}

    /**
     * Test JavaScript Service
     */
    @Service(
        name = "testJavaScript",
        engine = "javascript",
        location = "component://common/script/org/ofbiz/common/JavaScriptTest.js",
        description = "Test JavaScript Service",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "result", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface TestJavaScript {}

    /**
     * Cause a Referential Integrity Error
     */
    @Service(
        name = "testEntityFailure",
        location = "org.ofbiz.common.CommonServices",
        invoke = "entityFailTest",
        description = "Cause a Referential Integrity Error",
        validate = "false"
    )
    public interface TestEntityFailure {}

    /**
     * Test Entity Comparable
     */
    @Service(
        name = "entitySortTest",
        location = "org.ofbiz.common.CommonServices",
        invoke = "entitySortTest",
        description = "Test Entity Comparable",
        validate = "false"
    )
    public interface EntitySortTest {}

    /**
     * Test JavaScript Service
     */
    @Service(
        name = "makeALotOfVisits",
        location = "org.ofbiz.common.CommonServices",
        invoke = "makeALotOfVisits",
        description = "Test JavaScript Service",
        auth = "true",
        attributes = {
            @Attribute(name = "count", type = "Integer", mode = "IN"),
            @Attribute(name = "rollback", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "randomUserCount", type = "Integer", mode = "IN", optional = "true", description = "SCIPIO: If non-zero, each Visit will get a random userLoginId and partyId \n                from the system, with up to this count of possible different values (never null) (added 2018-02-15)")
        }
    )
    public interface MakeALotOfVisits {}

    /**
     * Test Passing ByteBuffer To Service
     */
    @Service(
        name = "byteBufferTest",
        location = "org.ofbiz.common.CommonServices",
        invoke = "byteBufferTest",
        description = "Test Passing ByteBuffer To Service",
        auth = "true",
        attributes = {
            @Attribute(name = "byteBuffer1", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "saveAsFileName1", type = "String", mode = "IN"),
            @Attribute(name = "byteBuffer2", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "saveAsFileName2", type = "String", mode = "IN")
        }
    )
    public interface ByteBufferTest {}

    /**
     * Upload Content Test Service
     */
    @Service(
        name = "uploadContentTest",
        location = "org.ofbiz.common.CommonServices",
        invoke = "uploadTest",
        description = "Upload Content Test Service",
        auth = "true",
        attributes = {
            @Attribute(name = "uploadFile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "_uploadFile_contentType", type = "String", mode = "IN"),
            @Attribute(name = "_uploadFile_fileName", type = "String", mode = "IN")
        }
    )
    public interface UploadContentTest {}

    /**
     * ECA Condition Service - Return TRUE
     */
    @Service(
        name = "conditionReturnTrue",
        location = "org.ofbiz.common.CommonServices",
        invoke = "conditionTrueService",
        description = "ECA Condition Service - Return TRUE",
        implemented = {@Implements(service = "serviceEcaConditionInterface")}
    )
    public interface ConditionReturnTrue {}

    /**
     * ECA Condition Service - Return FALSE
     */
    @Service(
        name = "conditionReturnFalse",
        location = "org.ofbiz.common.CommonServices",
        invoke = "conditionFalseService",
        description = "ECA Condition Service - Return FALSE",
        implemented = {@Implements(service = "serviceEcaConditionInterface")}
    )
    public interface ConditionReturnFalse {}

    @Service(
        name = "serviceStreamTest",
        location = "org.ofbiz.common.CommonServices",
        invoke = "streamTest",
        implemented = {@Implements(service = "serviceStreamInterface")}
    )
    public interface ServiceStreamTest {}

    /**
     * Test Ping Service
     */
    @Service(
        name = "ping",
        location = "org.ofbiz.common.CommonServices",
        invoke = "ping",
        description = "Test Ping Service",
        export = "true",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "message", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface Ping {}

}
