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
package com.ilscipio.scipio.commerceprofile;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

/**
 * SCIPIO: 4.0.0: Decisions of the hosted profile. One rule per class of service. The service ECA rules in
 * {@code servicedef/secas.xml} run the guard services at the ECA event {@code auth}, the first event of a service call,
 * for every caller: an MCP token, a web screen, a gateway call or a job. In the pooled runtime the rules are absolute
 * (blueprint G15). The caller is the {@code userLogin} of the call; a call that has only {@code login.username} and
 * {@code login.password} has none at that point and counts as not privileged.
 *
 * <p>Who passes: the internal {@code system} login and a login with {@code HOSTED_OPS} (the operators of the
 * {@code SCIPIO_OPS} group). No store group holds {@code HOSTED_OPS}.</p>
 */
public final class HostedGuard {

    public static final String OPS_PERMISSION = "HOSTED_OPS";
    public static final String CMS_CODE_ENTITY = "CMS_CODE";
    public static final String SYSTEM_LOGIN = "system";

    /** Parameters that let a caller pick the mail server or its credentials: refused when not empty. */
    public static final List<String> MAIL_PARAMS = Collections.unmodifiableList(Arrays.asList(
            "sendVia", "authUser", "authPass", "port", "sendType", "socketFactoryClass", "socketFactoryPort",
            "socketFactoryFallback", "customHeaders"));

    /** Parameters that let a caller pick a path on the server disk. */
    public static final List<String> FILE_PARAMS = Collections.unmodifiableList(Arrays.asList("filePath", "rootDir"));

    /** Files that a store may import with entityImport (setup wizard), relative to ofbiz.home. */
    public static final String IMPORT_ALLOW_PROPERTY = "hosted.import.allow";

    public enum Rule {
        /** A service that runs code, loads data, schedules jobs or writes files: operators and the system only. */
        CODE,
        /** A CMS service that takes a template body, a script or a template location: needs {@code CMS_CODE_UPDATE}. */
        CMS_CODE,
        /** A CMS service that returns code: needs {@code CMS_CODE_VIEW} (or {@code CMS_CODE_UPDATE}). */
        CMS_VIEW,
        /** A mail service: the caller may not pick the mail server. */
        MAIL_PARAMS,
        /** {@code entityImport}: only the files of the setup wizard. */
        IMPORT,
        /** {@code createFileFromScreen}: no path, a plain file name, a component screen. */
        SCREEN_FILE
    }

    /** Test seam: replaces the import allow list (null = property). */
    static volatile List<String> importAllowOverride;

    private HostedGuard() {}

    /** Returns null when the call may run, else the reason. Always null when the profile is not hosted. */
    public static String denyReason(Rule rule, GenericValue userLogin, Map<String, ?> context, Security security) {
        if (!HostedProfile.isHosted()) return null;
        if (isOperator(userLogin, security)) return null;
        switch (rule) {
        case CMS_CODE:
            if (hasCmsCode(userLogin, security, "_UPDATE")) return null;
            return "Editing template, script or asset code is not available in a hosted store.";
        case CMS_VIEW:
            if (hasCmsCode(userLogin, security, "_VIEW") || hasCmsCode(userLogin, security, "_UPDATE")) return null;
            return "Reading template, script or asset code is not available in a hosted store.";
        case MAIL_PARAMS:
            return mailReason(context);
        case IMPORT:
            return importReason(context);
        case SCREEN_FILE:
            return screenFileReason(context);
        case CODE:
        default:
            return "This action is not available in a hosted store.";
        }
    }

    private static boolean hasCmsCode(GenericValue userLogin, Security security, String action) {
        return userLogin != null && security != null && security.hasEntityPermission(CMS_CODE_ENTITY, action, userLogin);
    }

    static boolean isSet(Object value) {
        if (value == null) return false;
        if (value instanceof CharSequence) return !value.toString().trim().isEmpty();
        if (value instanceof Map) return !((Map<?, ?>) value).isEmpty();
        if (value instanceof Collection) return !((Collection<?>) value).isEmpty();
        return true;
    }

    private static boolean isTrue(Object value) {
        if (value instanceof Boolean) return (Boolean) value;
        if (value == null) return false;
        String s = value.toString().trim();
        return s.equalsIgnoreCase("Y") || s.equalsIgnoreCase("true");
    }

    private static String mailReason(Map<String, ?> context) {
        if (context == null) return null;
        for (String name : MAIL_PARAMS) {
            if (isSet(context.get(name))) return "Mail server settings cannot be set in a hosted store (parameter " + name + ").";
        }
        if (isTrue(context.get("allowCustomHeaders"))) {
            return "Custom mail headers cannot be set in a hosted store (parameter allowCustomHeaders).";
        }
        return null;
    }

    private static String importReason(Map<String, ?> context) {
        String msg = "This data import is not available in a hosted store.";
        if (context == null) return msg;
        if (isSet(context.get("fulltext")) || isSet(context.get("fmfilename")) || isTrue(context.get("isUrl"))) return msg;
        Object filename = context.get("filename");
        if (!isSet(filename)) return msg;
        try {
            String home = System.getProperty("ofbiz.home");
            if (home == null) return msg;
            String actual = new File(filename.toString()).getCanonicalPath();
            for (String rel : importAllow()) {
                if (actual.equals(new File(home, rel).getCanonicalPath())) return null;
            }
        } catch (IOException e) {
            return msg;
        }
        return msg;
    }

    static List<String> importAllow() {
        List<String> o = importAllowOverride;
        if (o != null) return o;
        List<String> out = new ArrayList<>();
        String v = UtilProperties.getPropertyValue(HostedProfile.RESOURCE, IMPORT_ALLOW_PROPERTY);
        if (v != null) for (String s : v.split(",")) if (!s.trim().isEmpty()) out.add(s.trim());
        return out;
    }

    private static String screenFileReason(Map<String, ?> context) {
        if (context == null) return null;
        for (String name : FILE_PARAMS) {
            if (isSet(context.get(name))) return "A file path cannot be set in a hosted store (parameter " + name + ").";
        }
        Object fileName = context.get("fileName");
        if (fileName != null) {
            String f = fileName.toString();
            if (f.contains("/") || f.contains("\\") || f.contains("..")) return "The file name may not hold a path (parameter fileName).";
        }
        Object screen = context.get("screenLocation");
        if (screen != null) {
            String s = screen.toString();
            if (!s.startsWith("component://") || s.contains("..")) return "The screen must be a component:// location (parameter screenLocation).";
        }
        return null;
    }

    /** True when {@code base/fileName} resolves to a place under {@code base}. */
    public static boolean staysUnder(String base, String fileName) {
        try {
            String root = new File(base).getCanonicalPath();
            String actual = new File(base, fileName == null ? "x" : fileName + "x").getCanonicalPath();
            return actual.startsWith(root + File.separator);
        } catch (IOException e) {
            return false;
        }
    }

    public static boolean isOperator(GenericValue userLogin, Security security) {
        if (userLogin == null) return false;
        if (SYSTEM_LOGIN.equals(userLogin.getString("userLoginId"))) return true;
        return security != null && security.hasPermission(OPS_PERMISSION, userLogin);
    }
}
