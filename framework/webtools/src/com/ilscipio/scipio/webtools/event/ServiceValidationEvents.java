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
package com.ilscipio.scipio.webtools.event;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelPermGroup;
import org.ofbiz.service.ModelPermission;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ModelServiceReader;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.group.GroupModel;
import org.ofbiz.service.group.GroupServiceModel;
import org.ofbiz.service.group.ServiceGroupReader;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;

/**
 * Runtime service-definition validator.
 *
 * <p>Walks every live {@link ModelService} definition known to the {@link DispatchContext} (the same set exposed by
 * the webtools "Available Services" page) and, per declared engine, verifies that the definition's target
 * (Java class/method, simple-method resource, script resource, service group, entity, permission-service, ...)
 * actually resolves. Unlike unit tests, this never invokes a service - it only mirrors the resolution logic each
 * service engine performs immediately before invocation, so a service that "validates" here is guaranteed to at
 * least reach its handler, not that the handler behaves correctly.</p>
 *
 * <p>Also cross-checks component service annotations ({@code @Service}) against the merge rule applied in
 * {@link DispatchContext#getGlobalServiceMap()}: an annotation-defined service whose name is also defined by an
 * XML/minilang service def is silently dropped in favor of the XML def. Those are reported as SHADOWED_BY_XML so
 * that annotation-only migration leftovers can be found and cleaned up (this is migration debt, not a runtime
 * error - the live service still works, just from the other definition).</p>
 *
 * <p>Writes a CSV report to {@code runtime/logs/service-validation-<yyyyMMdd-HHmmss>.csv} and returns a short
 * histogram summary as the event message.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ServiceValidationEvents {

    private static final String MODULE = ServiceValidationEvents.class.getName();

    private static final List<String> ENTITY_AUTO_INVOKES = Arrays.asList("create", "update", "delete", "expire");

    private ServiceValidationEvents() {}

    /**
     * Validates every live service definition and writes a CSV report to runtime/logs.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String validateAllServices(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        DispatchContext dctx = dispatcher.getDispatchContext();
        ClassLoader classLoader = dctx.getClassLoader();

        List<String[]> rows = new ArrayList<>();
        Map<String, Integer> histogram = new TreeMap<>();

        Set<String> serviceNames = dctx.getAllServiceNames(); // SCIPIO: NOTE: TreeSet, already sorted
        for (String serviceName : serviceNames) {
            try {
                ModelService model = dctx.getModelService(serviceName);
                validateService(dctx, delegator, classLoader, model, rows, histogram);
            } catch (Exception e) {
                addRow(rows, histogram, serviceName, "", "", "", "", "", "CHECK_ERROR",
                        e.getClass().getName() + ": " + e.getMessage());
            }
        }

        try {
            checkShadowedByXml(dctx, delegator, rows, histogram);
        } catch (Exception e) {
            // SCIPIO: The shadowed-by-XML enumeration is a best-effort aggregate check; a failure here must not
            // prevent the CSV report of the (already-collected) per-service rows from being written
            Debug.logError(e, "ServiceValidationEvents: SHADOWED_BY_XML enumeration failed", MODULE);
        }

        String timestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
        String csvRelPath = "runtime/logs/service-validation-" + timestamp + ".csv";
        String detail;
        try {
            writeCsv(csvRelPath, rows);
            detail = csvRelPath;
        } catch (IOException e) {
            Debug.logError(e, "ServiceValidationEvents: Could not write CSV report to " + csvRelPath, MODULE);
            detail = "FAILED TO WRITE CSV (" + e.getMessage() + ")";
        }

        StringBuilder summary = new StringBuilder();
        summary.append("Service validation complete: ").append(rows.size()).append(" rows checked. ");
        for (Map.Entry<String, Integer> entry : histogram.entrySet()) {
            summary.append(entry.getKey()).append("=").append(entry.getValue()).append(" ");
        }
        summary.append("CSV: ").append(detail);

        Debug.logInfo("ServiceValidationEvents: " + summary, MODULE);
        request.setAttribute("_EVENT_MESSAGE_", summary.toString());
        return "success";
    }

    /**
     * Validates a single service definition per its declared engine, then (if the engine-level check passed)
     * cross-checks any referenced permission-service(s), and appends exactly one CSV row for it.
     */
    private static void validateService(DispatchContext dctx, Delegator delegator, ClassLoader classLoader,
            ModelService m, List<String[]> rows, Map<String, Integer> histogram) {
        String engine = m.engineName;
        Verdict verdict;
        if (UtilValidate.isEmpty(engine)) {
            verdict = new Verdict("UNCHECKED_ENGINE", "no engine specified");
        } else {
            switch (engine) {
            case "java":
                verdict = checkJavaEngine(classLoader, m);
                break;
            case "simple":
                verdict = checkSimpleEngine(classLoader, m);
                break;
            case "groovy":
            case "script":
            case "javascript":
                verdict = checkScriptEngine(classLoader, m);
                break;
            case "group":
                verdict = checkGroupEngine(dctx, m);
                break;
            case "entity-auto":
                verdict = checkEntityAutoEngine(delegator, m);
                break;
            case "interface":
                verdict = new Verdict("SKIP_INTERFACE", null);
                break;
            default:
                verdict = new Verdict("UNCHECKED_ENGINE", "unrecognized engine [" + engine + "]");
            }
        }

        // SCIPIO: Permission-service cross-check only makes sense once the service itself resolves
        if ("OK".equals(verdict.code)) {
            List<String> missingPermServices = findMissingPermissionServices(dctx, m);
            if (!missingPermServices.isEmpty()) {
                verdict = new Verdict("MISSING_PERM_SERVICE",
                        "missing permission service(s): " + String.join(",", missingPermServices));
            }
        }

        addRow(rows, histogram, m.name, engine, m.location, m.invoke, m.fromLoader, m.getRelativeDefinitionLocation(),
                verdict.code, verdict.detail);
    }

    /**
     * Mirrors {@code StandardJavaEngine.getHandlerMethod}/{@code invokeHandlerMethod} resolution
     * (framework/service/src/org/ofbiz/service/engine/StandardJavaEngine.java ~L190-231): loads
     * {@code m.location} via the DispatchContext classloader, then tries the method signatures
     * StandardJavaEngine itself tries, in the same order: (DispatchContext, Map), (ServiceContext), no-arg.
     */
    private static Verdict checkJavaEngine(ClassLoader classLoader, ModelService m) {
        if (UtilValidate.isEmpty(m.location) || UtilValidate.isEmpty(m.invoke)) {
            return withEventPackageNote(new Verdict("MISSING_METHOD", "location and/or invoke missing"), m.location);
        }

        Class<?> clazz;
        try {
            clazz = classLoader.loadClass(m.location);
        } catch (ClassNotFoundException e) {
            return withEventPackageNote(new Verdict("MISSING_CLASS", e.toString()), m.location);
        } catch (Throwable t) { // SCIPIO: e.g. ExceptionInInitializerError, NoClassDefFoundError
            return withEventPackageNote(new Verdict("MISSING_CLASS", t.getClass().getSimpleName() + ": " + t.getMessage()), m.location);
        }

        Method method;
        try {
            method = clazz.getMethod(m.invoke, DispatchContext.class, Map.class);
        } catch (NoSuchMethodException e1) {
            try {
                method = clazz.getMethod(m.invoke, ServiceContext.class);
            } catch (NoSuchMethodException e2) {
                try {
                    method = clazz.getMethod(m.invoke);
                } catch (NoSuchMethodException e3) {
                    try {
                        clazz.getMethod(m.invoke, HttpServletRequest.class, HttpServletResponse.class);
                        return withEventPackageNote(
                                new Verdict("EVENT_SIGNATURE", "method [" + m.invoke + "] takes (HttpServletRequest, HttpServletResponse)"),
                                m.location);
                    } catch (NoSuchMethodException e4) {
                        return withEventPackageNote(
                                new Verdict("MISSING_METHOD", "no method [" + m.invoke + "] with an accepted service signature found on " + clazz.getName()),
                                m.location);
                    }
                }
            }
        }

        if (!Map.class.isAssignableFrom(method.getReturnType())) {
            return withEventPackageNote(
                    new Verdict("BAD_RETURN", "return type [" + method.getReturnType().getName() + "] not assignable to java.util.Map"),
                    m.location);
        }
        return withEventPackageNote(new Verdict("OK", null), m.location);
    }

    /** Appends the "in-event-package" note to the verdict detail when the location is in an ".event." package. */
    private static Verdict withEventPackageNote(Verdict verdict, String location) {
        if (location != null && location.matches(".*\\.event\\..*")) {
            verdict.detail = UtilValidate.isNotEmpty(verdict.detail) ? verdict.detail + "; in-event-package" : "in-event-package";
        }
        return verdict;
    }

    /**
     * Mirrors SimpleServiceEngine resolution (framework/minilang/src/org/ofbiz/minilang/SimpleServiceEngine.java
     * ~L79, package org.ofbiz.minilang - not org.ofbiz.service.engine as the resource location was named to be):
     * resolves m.location via FlexibleLocation and confirms it opens, then looks up the simple-method named
     * m.invoke via {@link SimpleMethod#getSimpleMethod(String, String, ClassLoader)}.
     */
    private static Verdict checkSimpleEngine(ClassLoader classLoader, ModelService m) {
        if (UtilValidate.isEmpty(m.location) || UtilValidate.isEmpty(m.invoke)) {
            return new Verdict("MISSING_RESOURCE", "location and/or invoke missing");
        }

        URL url;
        try {
            url = FlexibleLocation.resolveLocation(m.location, classLoader);
        } catch (MalformedURLException e) {
            return new Verdict("MISSING_RESOURCE", e.getMessage());
        }
        if (url == null) {
            return new Verdict("MISSING_RESOURCE", "could not resolve location [" + m.location + "]");
        }
        try (InputStream is = url.openStream()) {
            // SCIPIO: just confirming it opens/is readable
        } catch (IOException e) {
            return new Verdict("MISSING_RESOURCE", "not readable: " + e.getMessage());
        }

        SimpleMethod simpleMethod;
        try {
            simpleMethod = SimpleMethod.getSimpleMethod(m.location, m.invoke, classLoader);
        } catch (MiniLangException e) {
            // SCIPIO: resource resolved/opened above but failed to parse as simple-method XML; treat as a
            // resource-level problem since no distinct verdict is defined for parse failures
            return new Verdict("MISSING_RESOURCE", "error parsing simple-method XML: " + e.getMessage());
        }
        if (simpleMethod == null) {
            return new Verdict("MISSING_METHOD", "no <simple-method name=\"" + m.invoke + "\"> found in " + m.location);
        }
        return new Verdict("OK", null);
    }

    /**
     * Mirrors ScriptEngine resource resolution used by the groovy/script/javascript engines: only the resource
     * needs to resolve and open - the interpreter locates the invoked method/closure at call time, so there is no
     * static "method exists" check available here.
     */
    private static Verdict checkScriptEngine(ClassLoader classLoader, ModelService m) {
        if (UtilValidate.isEmpty(m.location)) {
            return new Verdict("MISSING_RESOURCE", "location missing");
        }
        URL url;
        try {
            url = FlexibleLocation.resolveLocation(m.location, classLoader);
        } catch (MalformedURLException e) {
            return new Verdict("MISSING_RESOURCE", e.getMessage());
        }
        if (url == null) {
            return new Verdict("MISSING_RESOURCE", "could not resolve location [" + m.location + "]");
        }
        try (InputStream is = url.openStream()) {
            // SCIPIO: just confirming it opens/is readable
        } catch (IOException e) {
            return new Verdict("MISSING_RESOURCE", "not readable: " + e.getMessage());
        }
        return new Verdict("OK", null);
    }

    /**
     * Mirrors ServiceGroupEngine resolution (framework/service/src/org/ofbiz/service/group/ServiceGroupEngine.java):
     * uses {@code m.internalGroup} when present (annotation-defined groups build the GroupModel in-memory), else
     * falls back to {@link ServiceGroupReader#getGroupModel(String)} keyed by m.location, then confirms every
     * member service resolves via {@link DispatchContext#getModelServiceOrNull(String)}.
     */
    private static Verdict checkGroupEngine(DispatchContext dctx, ModelService m) {
        GroupModel groupModel = m.internalGroup;
        if (groupModel == null) {
            if (UtilValidate.isEmpty(m.location)) {
                return new Verdict("MISSING_GROUP", "no internal group and no location");
            }
            groupModel = ServiceGroupReader.getGroupModel(m.location);
        }
        if (groupModel == null) {
            return new Verdict("MISSING_GROUP", "group [" + m.location + "] not found");
        }

        List<String> missingMembers = new ArrayList<>();
        for (GroupServiceModel gsm : groupModel.getServices()) {
            if (dctx.getModelServiceOrNull(gsm.getName()) == null) {
                missingMembers.add(gsm.getName());
            }
        }
        if (!missingMembers.isEmpty()) {
            return new Verdict("MISSING_MEMBER", "missing member service(s): " + String.join(",", missingMembers));
        }
        return new Verdict("OK", null);
    }

    /**
     * Mirrors EntityAutoEngine's checks (framework/service/src/org/ofbiz/service/engine/EntityAutoEngine.java
     * ~L79-90): invoke must be one of create/update/delete/expire, and defaultEntityName must resolve via
     * {@link Delegator#getModelEntity(String)}.
     */
    private static Verdict checkEntityAutoEngine(Delegator delegator, ModelService m) {
        if (UtilValidate.isEmpty(m.invoke) || !ENTITY_AUTO_INVOKES.contains(m.invoke)) {
            return new Verdict("BAD_INVOKE", "invoke [" + m.invoke + "] must be one of create/update/delete/expire");
        }
        if (UtilValidate.isEmpty(m.defaultEntityName)) {
            return new Verdict("MISSING_ENTITY", "default-entity-name not specified");
        }
        if (delegator.getModelEntity(m.defaultEntityName) == null) {
            return new Verdict("MISSING_ENTITY", "entity [" + m.defaultEntityName + "] not found");
        }
        return new Verdict("OK", null);
    }

    /**
     * Cross-checks the permission-service reference(s) of a service (top-level short form and any
     * permission-group entries of type permission-service; see org.ofbiz.service.ModelService#permissionServiceName
     * and org.ofbiz.service.ModelPermission#PERMISSION_SERVICE) against the live service map.
     */
    private static List<String> findMissingPermissionServices(DispatchContext dctx, ModelService m) {
        List<String> missing = new ArrayList<>();
        if (UtilValidate.isNotEmpty(m.permissionServiceName)
                && dctx.getModelServiceOrNull(m.permissionServiceName) == null) {
            missing.add(m.permissionServiceName);
        }
        for (ModelPermGroup group : m.getPermissionGroups()) {
            for (ModelPermission perm : group.permissions) {
                if (perm.permissionType == ModelPermission.PERMISSION_SERVICE
                        && UtilValidate.isNotEmpty(perm.permissionServiceName)
                        && dctx.getModelServiceOrNull(perm.permissionServiceName) == null) {
                    missing.add(perm.permissionServiceName);
                }
            }
        }
        return missing;
    }

    /**
     * Mirrors the annotation-vs-XML merge rule in {@link DispatchContext#getGlobalServiceMap()}
     * (framework/service/src/org/ofbiz/service/DispatchContext.java ~L433-442): an annotation-based service def
     * ({@code fromLoader == "annotations"}) is skipped by that merge when an XML/minilang def of the same name
     * already exists, so the live service map holds the XML def instead. This re-derives the raw per-component
     * annotation-only service maps (same call the merge loop uses: {@link ModelServiceReader#getModelServiceMap(
     * ComponentReflectInfo, Delegator)} over {@link ComponentReflectRegistry#getReflectInfos()}) and reports every
     * name whose live definition is not the annotation one.
     */
    private static void checkShadowedByXml(DispatchContext dctx, Delegator delegator, List<String[]> rows,
            Map<String, Integer> histogram) {
        Map<String, ModelService> annotationServices = new TreeMap<>();
        for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
            try {
                Map<String, ModelService> map = ModelServiceReader.getModelServiceMap(cri, delegator);
                if (map != null) {
                    annotationServices.putAll(map);
                }
            } catch (Exception e) {
                Debug.logWarning(e, "ServiceValidationEvents: Could not read service annotations for component reflect info; skipping for SHADOWED_BY_XML check", MODULE);
            }
        }

        for (Map.Entry<String, ModelService> entry : annotationServices.entrySet()) {
            String serviceName = entry.getKey();
            ModelService annotationModel = entry.getValue();
            ModelService live = dctx.getModelServiceOrNull(serviceName);
            if (live != null && !"annotations".equals(live.fromLoader)) {
                addRow(rows, histogram, serviceName, annotationModel.engineName, annotationModel.location,
                        annotationModel.invoke, annotationModel.fromLoader, annotationModel.getRelativeDefinitionLocation(),
                        "SHADOWED_BY_XML", live.getRelativeDefinitionLocation());
            }
        }
    }

    private static void addRow(List<String[]> rows, Map<String, Integer> histogram, String serviceName, String engine,
            String location, String invoke, String fromLoader, String definitionLocation, String verdict, String detail) {
        rows.add(new String[] { nullToEmpty(serviceName), nullToEmpty(engine), nullToEmpty(location),
                nullToEmpty(invoke), nullToEmpty(fromLoader), nullToEmpty(definitionLocation), nullToEmpty(verdict),
                nullToEmpty(detail) });
        histogram.merge(verdict, 1, Integer::sum);
    }

    private static String nullToEmpty(String value) {
        return value != null ? value : "";
    }

    /**
     * Writes the CSV report. Resolves the runtime/logs-relative path the same way other webtools code does
     * (see framework/webtools/webapp/webtools/WEB-INF/actions/log/LogView.groovy, which resolves the
     * "runtime/logs/ofbiz.log" property value via {@link FileUtil#getFile(String)}) - no hardcoded ofbiz.home.
     */
    private static void writeCsv(String csvRelPath, List<String[]> rows) throws IOException {
        File csvFile = FileUtil.getFile(csvRelPath);
        File parentDir = csvFile.getParentFile();
        if (parentDir != null && !parentDir.exists()) {
            parentDir.mkdirs();
        }
        try (PrintWriter writer = new PrintWriter(new OutputStreamWriter(
                new java.io.FileOutputStream(csvFile), StandardCharsets.UTF_8))) {
            writer.print("serviceName,engine,location,invoke,fromLoader,definitionLocation,verdict,detail\r\n");
            for (String[] row : rows) {
                StringBuilder line = new StringBuilder();
                for (int i = 0; i < row.length; i++) {
                    if (i > 0) {
                        line.append(",");
                    }
                    line.append(csvEscape(row[i]));
                }
                line.append("\r\n");
                writer.print(line);
            }
        }
    }

    private static String csvEscape(String value) {
        if (value == null) {
            return "";
        }
        if (value.indexOf(',') >= 0 || value.indexOf('"') >= 0 || value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0) {
            return "\"" + value.replace("\"", "\"\"") + "\"";
        }
        return value;
    }

    /** Small mutable holder for a per-service check outcome. */
    private static final class Verdict {
        private final String code;
        private String detail;

        private Verdict(String code, String detail) {
            this.code = code;
            this.detail = detail;
        }
    }
}
