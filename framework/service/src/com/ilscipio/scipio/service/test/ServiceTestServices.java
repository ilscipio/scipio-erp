package com.ilscipio.scipio.service.test;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.EntityAttributes;
import com.ilscipio.scipio.service.def.Implements;
import com.ilscipio.scipio.service.def.Service;
import org.ofbiz.base.util.Debug;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.util.Map;

/**
 * Test services.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support (demo/test).</p>
 */
public abstract class ServiceTestServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Service(
            attributes = {
                    @Attribute(name = "param1", type = "String", mode = "IN", defaultValue = "test value 1", optional = "true")
            }
    )
    public static Map<String, Object> adminTestAnnotations1(ServiceContext ctx) {
        Debug.logInfo("adminTestAnnotations1: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

    @Service(
            attributes = {
                    @Attribute(name = "param1", type = "String", mode = "IN", defaultValue = "test value 1", optional = "true"),
                    @Attribute(name = "param2", type = "String", mode = "IN", defaultValue = "test value 2", optional = "true"),
                    @Attribute(name = "result1", type = "String", mode = "OUT", optional = "true")
            }
    )
    public static Map<String, Object> adminTestAnnotations2(ServiceContext ctx) {
        Debug.logInfo("adminTestAnnotations2: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

    @Service(
            name = "adminTestAnnotations2Extended",
            description = "Extended @Service annotations test service (2)",
            deprecated = "Test deprecation message",
            deprecatedSince = "2023-02-10",
            deprecatedBy = "adminTestAnnotations1",
            implemented = {
                    @Implements(service = "adminTestAnnotations1"),
                    @Implements(service = "adminTestAnnotations2")
            },
            defaultEntityName = "Person",
            entityAttributes = {
                    @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
                    @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
            },
            attributes = {
                    @Attribute(name = "param3", type = "String", mode = "IN", defaultValue = "test value 3", optional = "true"),
                    @Attribute(name = "param4", type = "String", mode = "IN", defaultValue = "test value 4", optional = "true"),
                    @Attribute(name = "result2", type = "String", mode = "OUT", optional = "true")
            },
            auth = "true"
    )
    public static Map<String, Object> adminTestAnnotations2Ext(ServiceContext ctx) {
        Debug.logInfo("adminTestAnnotations2Ext: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

}
