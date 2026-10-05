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
package com.ilscipio.scipio.cms.demo;

import java.io.File;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.entity.demo.DemoReset;

/**
 * SCIPIO: 4.0.0: Public demo: puts the CMS back to the data files (service cmsDemoReset, hourly job CMS_DEMO_RESET).
 * <p>
 * The CMS set is every row of the Cms* entities, the Content they point to (contentId, activeContentId), the CMS media
 * (contentTypeId SCP_MEDIA, SCP_MEDIA_VARIANT) and the associations, data resources and texts of that content. The
 * rows of this set in the data files are restored where a visitor changed or deleted them; rows of the set that are
 * not in the data files (pages, versions, templates, mappings, media that visitors added) are deleted. Off unless
 * general.properties demo.reset.enabled=true (see {@link DemoReset}).
 */
public final class CmsDemoReset {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Restore order: each row after the rows it points to. Deletes go in the reverse order. */
    private static final List<String> ORDER = List.of(
            "DataResource", "ElectronicText", "ImageDataResource", "VideoDataResource", "AudioDataResource", "DocumentDataResource",
            "OtherDataResource", "DataResourceAttribute", "Content", "ContentAssoc",
            "CmsScriptTemplate", "CmsPageTemplate", "CmsAssetTemplate", "CmsPage", "CmsMenu",
            "CmsPageTemplateVersion", "CmsAssetTemplateVersion", "CmsPageVersion", "CmsAttributeTemplate",
            "CmsPageTemplateAssetAssoc", "CmsPageTemplateScriptAssoc", "CmsAssetTemplateScriptAssoc", "CmsPageScriptAssoc",
            "CmsViewMapping", "CmsProcessMapping", "CmsProcessViewMapping", "CmsPageSpecialMapping", "CmsPageProductAssoc",
            "CmsPageAuthorization", "CmsAccessToken",
            "CmsPageVersionState", "CmsPageTemplateVersionState", "CmsAssetTemplateVersionState");
    private static final List<String> DATA_RESOURCE_PARTS = List.of("ElectronicText", "ImageDataResource", "VideoDataResource",
            "AudioDataResource", "DocumentDataResource", "OtherDataResource", "DataResourceAttribute");
    private static final List<String> MEDIA_TYPES = List.of("SCP_MEDIA", "SCP_MEDIA_VARIANT");
    private static final List<String> CONTENT_FIELDS = List.of("contentId", "activeContentId");
    /** Rows the content component derives from a Content (e.g. the keyword index of an EECA); removed with it. */
    private static final List<String> CONTENT_CHILDREN = List.of("ContentKeyword", "ContentAttribute", "ContentRole", "ContentMetaData", "ContentPurpose");
    private static final int DELETE_PASSES = 4;
    /** Removed rows are written here first (entity XML, importable), and kept for BACKUP_DAYS. */
    private static final String BACKUP_DIR = "runtime/demo-reset";
    private static final long BACKUP_DAYS = 7;

    private CmsDemoReset() {
    }

    /** Where the rows come from: the data files or the database. */
    private interface Source {
        List<GenericValue> all(String entityName) throws GenericEntityException;
        List<GenericValue> in(String entityName, String field, Collection<String> values) throws GenericEntityException;
    }

    public static Map<String, Object> cmsDemoReset(DispatchContext dctx, Map<String, ?> context) {
        if (!DemoReset.isEnabled()) {
            return ServiceUtil.returnSuccess("Demo reset is off (general.properties demo.reset.enabled)");
        }
        Delegator delegator = dctx.getDelegator();
        boolean dryRun = Boolean.TRUE.equals(context.get("dryRun"));
        long start = System.currentTimeMillis();
        try {
            Map<String, GenericValue> file = DemoReset.byPk(cmsRows(delegator, new Source() {
                @Override
                public List<GenericValue> all(String entityName) {
                    return DemoReset.getBaseline(delegator, entityName);
                }
                @Override
                public List<GenericValue> in(String entityName, String field, Collection<String> values) {
                    // immutable collections throw on contains(null)
                    return DemoReset.getBaseline(delegator, entityName).stream()
                            .filter(v -> v.getString(field) != null && values.contains(v.getString(field))).collect(Collectors.toList());
                }
            }));
            Map<String, GenericValue> db = DemoReset.byPk(cmsRows(delegator, new Source() {
                @Override
                public List<GenericValue> all(String entityName) throws GenericEntityException {
                    return EntityQuery.use(delegator).from(entityName).queryList();
                }
                @Override
                public List<GenericValue> in(String entityName, String field, Collection<String> values) throws GenericEntityException {
                    return dbIn(delegator, entityName, field, values);
                }
            }));

            // 1. restore the rows of the data files that visitors changed or deleted (parents first)
            List<GenericValue> restore = new ArrayList<>();
            for (String entityName : ORDER) {
                for (GenericValue fileValue : file.values()) {
                    if (fileValue.getEntityName().equals(entityName)) {
                        GenericValue dbValue = db.get(DemoReset.pkKey(fileValue));
                        if (dbValue == null || DemoReset.differs(fileValue, dbValue)) {
                            restore.add(fileValue);
                        }
                    }
                }
            }
            // 2. delete the rows that are not in the data files (children first; a row that another row still points to
            //    fails and is tried again in the next pass)
            List<GenericValue> remove = new ArrayList<>();
            for (int i = ORDER.size() - 1; i >= 0; i--) {
                for (GenericValue dbValue : db.values()) {
                    if (dbValue.getEntityName().equals(ORDER.get(i)) && !file.containsKey(DemoReset.pkKey(dbValue))) {
                        remove.add(dbValue);
                    }
                }
            }
            // rows that the content component derives from a Content (keyword index, attributes ...) are in no data file:
            // they go with their Content, before it
            Set<String> removedContentIds = remove.stream().filter(v -> "Content".equals(v.getEntityName()))
                    .map(v -> v.getString("contentId")).collect(Collectors.toSet());
            List<GenericValue> contentChildren = new ArrayList<>();
            for (String child : CONTENT_CHILDREN) {
                if (!removedContentIds.isEmpty() && delegator.getModelEntity(child) != null) {
                    contentChildren.addAll(dbIn(delegator, child, "contentId", removedContentIds));
                }
            }
            remove.addAll(0, contentChildren);
            if (dryRun) {
                Debug.logInfo("Demo reset (dry run): CMS would restore " + restore.size() + " rows " + summary(restore)
                        + " and remove " + remove.size() + " rows " + summary(remove), module);
                return result(restore.size(), remove.size(), 0);
            }
            int restored = 0;
            List<GenericValue> failed = new ArrayList<>();
            for (GenericValue fileValue : restore) {
                // create or update: a row can exist although no CMS row points to it (then it is not in the db map)
                if (inOwnTransaction(() -> {
                    delegator.createOrStore(delegator.makeValue(fileValue.getEntityName(), fileValue));
                    return true;
                })) {
                    restored++;
                } else {
                    failed.add(fileValue);
                }
            }
            if (!remove.isEmpty()) {
                backup(remove);
            }
            int removed = 0;
            for (int pass = 0; pass < DELETE_PASSES && !remove.isEmpty(); pass++) {
                List<GenericValue> again = new ArrayList<>();
                for (GenericValue dbValue : remove) {
                    if (inOwnTransaction(() -> {
                        dbValue.remove();
                        return true;
                    })) {
                        removed++;
                    } else {
                        again.add(dbValue);
                    }
                }
                remove = again;
            }
            failed.addAll(remove);

            // 3. caches: CMS objects, mappings, entity caches, image variants
            if (restored > 0 || removed > 0) {
                UtilCache.clearCachesThatStartWith("cms.");
                delegator.clearAllCaches();
                LocalDispatcher dispatcher = dctx.getDispatcher();
                for (String service : List.of("cmsClearMappingCaches", "contentImageVariantsClearCaches")) {
                    try {
                        dispatcher.runSync(service, UtilMisc.toMap("userLogin", context.get("userLogin")));
                    } catch (Exception e) {
                        Debug.logWarning("Demo reset: " + service + ": " + e.getMessage(), module);
                    }
                }
            }
            String msg = "Demo reset: CMS back to the data files: " + restored + " rows restored " + summary(restore) + ", " + removed
                    + " rows removed, " + failed.size() + " failed " + summary(failed) + " (" + (System.currentTimeMillis() - start) + " ms)";
            if (failed.isEmpty()) {
                Debug.logInfo(msg, module);
            } else {
                Debug.logWarning(msg, module);
            }
            return result(restored, removed, failed.size());
        } catch (GenericEntityException e) {
            Debug.logError(e, "Demo reset: CMS reset failed", module);
            return ServiceUtil.returnError("CMS demo reset failed: " + e.getMessage());
        }
    }

    /** The CMS set of a source (see the class comment). */
    private static List<GenericValue> cmsRows(Delegator delegator, Source source) throws GenericEntityException {
        List<GenericValue> out = new ArrayList<>();
        Set<String> contentIds = new HashSet<>();
        for (String entityName : ORDER) {
            if (!entityName.startsWith("Cms") || delegator.getModelEntity(entityName) == null) {
                continue;
            }
            for (GenericValue v : source.all(entityName)) {
                out.add(v);
                for (String field : CONTENT_FIELDS) {
                    if (v.getModelEntity().isField(field) && v.getString(field) != null) {
                        contentIds.add(v.getString(field));
                    }
                }
            }
        }
        List<GenericValue> contents = new ArrayList<>(source.in("Content", "contentTypeId", MEDIA_TYPES));
        contents.addAll(source.in("Content", "contentId", contentIds));
        Map<String, GenericValue> uniqueContents = DemoReset.byPk(contents);
        out.addAll(uniqueContents.values());
        Set<String> allContentIds = uniqueContents.values().stream().map(c -> c.getString("contentId")).collect(Collectors.toSet());
        Map<String, GenericValue> assocs = DemoReset.byPk(source.in("ContentAssoc", "contentId", allContentIds));
        assocs.putAll(DemoReset.byPk(source.in("ContentAssoc", "contentIdTo", allContentIds)));
        out.addAll(assocs.values());
        Set<String> dataResourceIds = uniqueContents.values().stream().map(c -> c.getString("dataResourceId"))
                .filter(id -> id != null).collect(Collectors.toSet());
        out.addAll(source.in("DataResource", "dataResourceId", dataResourceIds));
        for (String part : DATA_RESOURCE_PARTS) {
            if (delegator.getModelEntity(part) != null) {
                out.addAll(source.in(part, "dataResourceId", dataResourceIds));
            }
        }
        return out;
    }

    private static List<GenericValue> dbIn(Delegator delegator, String entityName, String field, Collection<String> values) throws GenericEntityException {
        List<GenericValue> out = new ArrayList<>();
        List<String> list = new ArrayList<>(values);
        for (int i = 0; i < list.size(); i += 500) {
            out.addAll(EntityQuery.use(delegator).from(entityName)
                    .where(EntityCondition.makeCondition(field, EntityOperator.IN, list.subList(i, Math.min(i + 500, list.size())))).queryList());
        }
        return out;
    }

    private interface Step {
        boolean run() throws GenericEntityException;
    }

    /** Runs a step in a transaction of its own, so that one failed row does not roll back the others. */
    private static boolean inOwnTransaction(Step step) {
        try {
            Boolean ok = TransactionUtil.doNewTransaction(step::run, "Demo reset step", 0, false);
            return Boolean.TRUE.equals(ok);
        } catch (GenericEntityException e) {
            return false;
        }
    }

    /** Writes the rows as entity XML to runtime/demo-reset/cms-removed-*.xml (webtools can import it again); drops old files. */
    private static void backup(List<GenericValue> rows) {
        File dir = new File(System.getProperty("ofbiz.home", "."), BACKUP_DIR);
        if (!dir.isDirectory() && !dir.mkdirs()) {
            Debug.logWarning("Demo reset: cannot create " + dir, module);
            return;
        }
        File[] old = dir.listFiles((d, name) -> name.startsWith("cms-removed-") && name.endsWith(".xml"));
        if (old != null) {
            for (File f : old) {
                if (f.lastModified() < System.currentTimeMillis() - BACKUP_DAYS * 24 * 3600 * 1000) {
                    f.delete();
                }
            }
        }
        File file = new File(dir, "cms-removed-" + new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date()) + ".xml");
        try (PrintWriter writer = new PrintWriter(file, StandardCharsets.UTF_8)) {
            writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            writer.println("<entity-engine-xml>");
            for (GenericValue row : rows) {
                row.writeXmlText(writer, "    ");
            }
            writer.println("</entity-engine-xml>");
        } catch (Exception e) {
            Debug.logWarning("Demo reset: cannot write " + file + ": " + e.getMessage(), module);
        }
    }

    private static Map<String, Object> result(int restored, int removed, int failed) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("restoredRows", restored);
        result.put("removedRows", removed);
        result.put("failedRows", failed);
        return result;
    }

    /** Row counts per entity, e.g. {CmsPageVersion=2, Content=2}. */
    private static String summary(Collection<GenericValue> values) {
        Map<String, Integer> counts = new HashMap<>();
        for (GenericValue v : values) {
            counts.merge(v.getEntityName(), 1, Integer::sum);
        }
        return counts.toString();
    }
}
