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
package com.ilscipio.scipio.countrypack.service;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;
import com.ilscipio.scipio.countrypack.core.PackEngine;
import com.ilscipio.scipio.countrypack.core.PackRegistry;
import com.ilscipio.scipio.countrypack.core.PackStore;
import com.ilscipio.scipio.countrypack.core.Role;
import com.ilscipio.scipio.countrypack.core.TaskStatus;
import com.ilscipio.scipio.countrypack.core.TemplateSource;
import com.ilscipio.scipio.countrypack.store.EntityPackStore;

/**
 * Service implementations of the country-pack framework: thin adapters between the service engine and {@link PackEngine}.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class CountryPackServiceImpl {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static volatile PackRegistry registry;

    private CountryPackServiceImpl() {
    }

    /** The packs of the component folder; read once. */
    public static PackRegistry registry() {
        PackRegistry r = registry;
        if (r == null) {
            synchronized (CountryPackServiceImpl.class) {
                if (registry == null) {
                    try {
                        registry = new PackRegistry(Path.of(ComponentConfig.getRootLocation("country-packs")));
                    } catch (ComponentException e) {
                        throw new IllegalStateException("Component country-packs not found", e);
                    }
                }
                r = registry;
            }
        }
        return r;
    }

    /** The engine for one delegator. Legal texts that a pack does not carry come from the shipped templates of the compliance component. */
    public static PackEngine engine(Delegator delegator, String userLoginId) {
        PackRegistry reg = registry();
        TemplateSource templates = new TemplateSource(reg, (slug, language) -> LegalDocumentWorker.getTemplateText(slug, new Locale(language)));
        return new PackEngine(reg, new EntityPackStore(delegator, userLoginId), templates);
    }

    private static boolean allowed(DispatchContext dctx, Map<String, ?> context, boolean write) {
        Security security = dctx.getSecurity();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        return write ? canWrite(security, userLogin) : canView(security, userLogin);
    }

    private static boolean canWrite(Security security, GenericValue userLogin) {
        return security.hasPermission("COUNTRYPACK_ADMIN", userLogin) || security.hasPermission("SETUP_ADMIN", userLogin)
                || security.hasPermission("COUNTRYPACK_UPDATE", userLogin);
    }

    /** Read access: COUNTRYPACK_VIEW, or a permission that allows a change (COUNTRYPACK_UPDATE, COUNTRYPACK_ADMIN, SETUP_ADMIN), or a SETUP_ view permission. */
    public static boolean canView(Security security, GenericValue userLogin) {
        return userLogin != null && (canWrite(security, userLogin) || security.hasPermission("COUNTRYPACK_VIEW", userLogin)
                || security.hasEntityPermission("SETUP", "_VIEW", userLogin));
    }

    /** The upgrade changes the assignments of stores: only COUNTRYPACK_ADMIN or an operator (group SCIPIO_OPS). */
    private static boolean isAdminOrOperator(DispatchContext dctx, Map<String, ?> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin == null) {
            return false;
        }
        if (dctx.getSecurity().hasPermission("COUNTRYPACK_ADMIN", userLogin)) {
            return true;
        }
        try {
            return EntityQuery.use(dctx.getDelegator()).from("UserLoginSecurityGroup")
                    .where("userLoginId", userLogin.getString("userLoginId"), "groupId", "SCIPIO_OPS").filterByDate().queryCount() > 0;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not read the security groups", module);
            return false;
        }
    }

    private static Map<String, Object> denied(boolean write) {
        return ServiceUtil.returnError("Permission " + (write ? "COUNTRYPACK_UPDATE" : "COUNTRYPACK_VIEW") + " is required.");
    }

    private static String login(Map<String, ?> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        return userLogin != null ? userLogin.getString("userLoginId") : null;
    }

    public static Map<String, Object> apply(DispatchContext dctx, Map<String, ? extends Object> context) {
        if (!allowed(dctx, context, true)) {
            return denied(true);
        }
        try {
            String roleId = (String) context.get("roleId");
            Role role = UtilValidate.isEmpty(roleId) ? null : Role.valueOf(roleId.trim().toUpperCase(Locale.ROOT));
            PackEngine.ApplyResult r = engine(dctx.getDelegator(), login(context)).apply((String) context.get("productStoreId"), (String) context.get("packId"), role);
            Map<String, Object> result = ServiceUtil.returnSuccess("Applied pack " + r.packId() + " as " + r.role());
            result.put("result", r.toMap());
            return result;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (RuntimeException e) {
            Debug.logError(e, "Could not apply the country pack", module);
            return ServiceUtil.returnError("Could not apply the country pack: " + e.getMessage());
        }
    }

    public static Map<String, Object> completeTask(DispatchContext dctx, Map<String, ? extends Object> context) {
        if (!allowed(dctx, context, true)) {
            return denied(true);
        }
        try {
            String statusId = (String) context.get("statusId");
            TaskStatus status = UtilValidate.isEmpty(statusId) ? null : TaskStatus.valueOf(statusId.trim().toUpperCase(Locale.ROOT));
            if (status == TaskStatus.DONE_BY_SCIPIO) {
                return ServiceUtil.returnError("The state DONE_BY_SCIPIO is set by Scipio only.");
            }
            PackEngine engine = engine(dctx.getDelegator(), login(context));
            String storeId = (String) context.get("productStoreId");
            PackStore.TaskRow row = engine.completeTask(storeId, (String) context.get("packId"), (String) context.get("taskId"), status,
                    (String) context.get("value"), (String) context.get("note"));
            Map<String, Object> result = ServiceUtil.returnSuccess("Task " + row.taskId() + " is " + row.status());
            result.put("marketOpen", engine.marketOpen(storeId, row.packId()));
            return result;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (RuntimeException e) {
            Debug.logError(e, "Could not change the task", module);
            return ServiceUtil.returnError("Could not change the task: " + e.getMessage());
        }
    }

    public static Map<String, Object> upgradeAll(DispatchContext dctx, Map<String, ? extends Object> context) {
        if (!isAdminOrOperator(dctx, context)) {
            return ServiceUtil.returnError("Permission COUNTRYPACK_ADMIN or the group SCIPIO_OPS is required.");
        }
        try {
            String onlyStoreId = UtilValidate.isEmpty((String) context.get("productStoreId")) ? null : (String) context.get("productStoreId");
            List<Map<String, Object>> results = new ArrayList<>();
            for (PackEngine.ApplyResult r : engine(dctx.getDelegator(), login(context)).upgradeAll(onlyStoreId)) {
                results.add(r.toMap());
            }
            Map<String, Object> result = ServiceUtil.returnSuccess("Upgraded " + results.size() + " pack assignments");
            result.put("results", results);
            return result;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (RuntimeException e) {
            Debug.logError(e, "Could not upgrade the country packs", module);
            return ServiceUtil.returnError("Could not upgrade the country packs: " + e.getMessage());
        }
    }

    public static Map<String, Object> status(DispatchContext dctx, Map<String, ? extends Object> context) {
        if (!allowed(dctx, context, false)) {
            return denied(false);
        }
        try {
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("status", engine(dctx.getDelegator(), login(context)).status((String) context.get("productStoreId")));
            return result;
        } catch (IllegalArgumentException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (RuntimeException e) {
            Debug.logError(e, "Could not read the country pack status", module);
            return ServiceUtil.returnError("Could not read the country pack status: " + e.getMessage());
        }
    }
}
