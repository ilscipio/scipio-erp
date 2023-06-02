package com.ilscipio.scipio.service.test;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.EntityAttributes;
import com.ilscipio.scipio.service.def.Implements;
import com.ilscipio.scipio.service.def.OverrideAttribute;
import com.ilscipio.scipio.service.def.Permission;
import com.ilscipio.scipio.service.def.PermissionService;
import com.ilscipio.scipio.service.def.Permissions;
import com.ilscipio.scipio.service.def.Service;
import org.ofbiz.base.util.Debug;
import org.ofbiz.service.LocalService;
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
                    @Attribute(name = "param1", type = "String", mode = "IN", defaultValue = "test value 1", optional = "true"),
                    @Attribute(name = "result1", type = "String", mode = "OUT", optional = "true")
            }
    )
    public static Map<String, Object> serviceAnnotationsTest1(ServiceContext ctx) {
        Debug.logInfo("serviceAnnotationsTest1: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

    // Shorthand version
    @Service
    @Attribute(name = "param1", type = "String", mode = "IN", defaultValue = "test value 1", optional = "true")
    @Attribute(name = "param2", type = "String", mode = "IN", defaultValue = "test value 2", optional = "true")
    @Attribute(name = "result1", type = "String", mode = "OUT", optional = "true")
    @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
    @Permission(permission = "ENTITYMAINT")
    public static class ServiceAnnotationsTest2 extends LocalService {
        @Override
        public Map<String, Object> exec() {
            Debug.logInfo("serviceAnnotationsTest2: input: " + ctx.context(), module);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("result1", "test result value 1");
            return result;
        }
    }

    @Service(
            implemented = {
                    @Implements(service = "serviceAnnotationsTest1")
            },
            auth = "true",
            permissionService = {
                    @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
            }
    )
    public static Map<String, Object> serviceAnnotationsTest3(ServiceContext ctx) {
        Debug.logInfo("serviceAnnotationsTest3: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

    // NOTE: @Permissions is required when you need to specify a joinType or multiple services
    @Service(auth = "true")
    @Implements(service = "serviceAnnotationsTest1")
    @Attribute(name = "param2b", type = "String", mode = "IN", defaultValue = "test value 2b", optional = "true")
    @Permissions(joinType = "AND",
            permissions = { @Permission(permission = "OFBTOOLS", action = "_VIEW"),
                            @Permission(permission = "ENTITY_MAINT") },
            services = { @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE") })
    public static Map<String, Object> serviceAnnotationsTest3b(ServiceContext ctx) {
        Debug.logInfo("serviceAnnotationsTest3b: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        return result;
    }

    @Service(
            name = "serviceAnnotationsTest4Extended",
            description = "Extended @Service annotations test service (2)",
            deprecated = "Test deprecation message",
            deprecatedSince = "2023-02-10",
            deprecatedBy = "serviceAnnotationsTest3",
            implemented = {
                    @Implements(service = "serviceAnnotationsTest1"),
                    @Implements(service = "serviceAnnotationsTest2")
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
            auth = "true",
            permissions = {
                    @Permissions(
                            joinType = "AND",
                            permissions = {
                                    @Permission(permission = "OFBTOOLS", action = "_VIEW")
                            },
                            services = {
                                    @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE")
                            }
                    )
            },
            log = "debug",
            overrideAttributes = {
                    @OverrideAttribute(name = "param1", defaultValue = "test value 1 override"),
                    @OverrideAttribute(name = "param2", defaultValue = "test value 2 override"),
            },
            logEca = "debug",
            maxRetry = "3",
            priority = "25",
            jobPoolPersist = "pool",
            requireNewTransaction = "true",
            validate = "true",
            semaphore = "fail",
            semaphoreSleep = "1000",
            semaphoreWaitSeconds = "800",
            startDelay = "200",
            hideResultInLog = "false"
    )
    public static Map<String, Object> serviceAnnotationsTest4Ext(ServiceContext ctx) {
        Debug.logInfo("serviceAnnotationsTest4Extended: input: " + ctx.context(), module);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("result1", "test result value 1");
        result.put("result2", "test result value 2");
        return result;
    }

}
