package com.ilscipio.scipio.service.test;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.EntityAttributes;
import com.ilscipio.scipio.service.def.Implements;
import com.ilscipio.scipio.service.def.OverrideAttribute;
import com.ilscipio.scipio.service.def.Permission;
import com.ilscipio.scipio.service.def.PermissionService;
import com.ilscipio.scipio.service.def.Permissions;
import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.eeca.Eeca;
import com.ilscipio.scipio.service.def.eeca.EecaAction;
import com.ilscipio.scipio.service.def.eeca.EecaSet;
import com.ilscipio.scipio.service.def.seca.Seca;
import com.ilscipio.scipio.service.def.seca.SecaAction;
import com.ilscipio.scipio.service.def.seca.SecaSet;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.service.LocalService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
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
            Map<String, Object> result = ctx.success();
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
    public static class ServiceAnnotationsTest3 extends LocalService {
        @Override
        public Map<String, Object> exec() {
            Debug.logInfo("serviceAnnotationsTest3: input: " + ctx.context(), module);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("result1", "test result value 1");
            return result;
        }
    }

    // NOTE: @Permissions is required when you need to specify a joinType or multiple services
    @Service(auth = "true")
    @Implements(service = "serviceAnnotationsTest1")
    @Attribute(name = "param2b", type = "String", mode = "IN", defaultValue = "test value 2b", optional = "true")
    @Permissions(joinType = "AND",
            permissions = { @Permission(permission = "OFBTOOLS", action = "_VIEW"),
                            @Permission(permission = "ENTITY_MAINT") },
            services = { @PermissionService(service = "commonGenericPermission", mainAction = "UPDATE") })
    public static class ServiceAnnotationsTest3b extends LocalService {
        @Override
        public Map<String, Object> exec() {
            Debug.logInfo("serviceAnnotationsTest3b: input: " + ctx.context(), module);
            Map<String, Object> result = ctx.success();
            result.put("result1", "test result value 1");
            return result;
        }
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

    // Injection, EECA and SECA test
    @Service(auth = "true")
    @Attribute(name = "stringParam1", typeCls = String.class, mode = "IN", defaultValue = "test value 1", optional = "true")
    @Attribute(name = "stringParam1b", typeCls = String.class, mode = "IN", defaultValue = "test value 1b", optional = "true")
    @Eeca(entity = "Testing", operation = "create-store", event = "return") // default mode: actions = {@EecaAction(mode = "sync")
    @Eeca(entity = "Testing", operation = "remove", event = "return", actions = {
            @EecaAction(mode = "async")})
    @Seca(service = "serviceAnnotationsTest3b", event = "global-commit")
    @Seca(service = "serviceAnnotationsTest4Extended", event = "commit", actions = {
            @SecaAction(mode = "async", assignments = {
                    @SecaSet(fieldName = "stringParam1b", value = "test value 1b from SECA")})})
    public static class ServiceAnnotationsTest4c extends LocalService {

        // Manually-assigned parameters
        protected String stringParam1;

        @Attribute(name = "stringParam2", typeCls = String.class, mode = "IN", defaultValue = "test value 2", optional = "true")
        protected String stringParam2;

        // Auto-injected parameters (inject = "true")

        @Attribute(name = "stringParam1b", inject = "true")
        protected String stringParam1b;

        @Attribute(name = "stringListParam3", typeCls = List.class, mode = "IN", defaultValue = "[testvalue3a, testvalue3b]", optional = "true", inject = "true")
        protected Collection<String> stringListParam3;

        @Attribute(name = "mapParam4", typeCls = Map.class, mode = "IN", defaultValue = "{testkey4a=testvalue4a, testkey4b=testvalue4b}", optional = "true", inject = "true")
        protected Map<String, Object> mapParam4;

        @Override
        public void init(ServiceContext ctx) throws GeneralException {
            stringParam1 = ctx.attr("stringParam1");
            stringParam2 = ctx.attr("stringParam2");
            super.init(ctx);
        }

        @Override
        public Map<String, Object> exec() {
            Debug.logInfo("serviceAnnotationsTest4c: input: " + ctx.context(), module);
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("stringParam1", stringParam1);
            params.put("stringParam2", stringParam2);
            params.put("stringParam1b", stringParam1b);
            params.put("stringListParam3", stringListParam3);
            params.put("mapParam4", mapParam4);
            Debug.logInfo("serviceAnnotationsTest4c: params: " + params, module);
            return ctx.success("Injected params: " + params);
        }
    }

}
