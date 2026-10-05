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
package org.ofbiz.entity.tenant;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.TenantScope;
import org.ofbiz.entity.util.Tenants;

/**
 * SCIPIO: 4.0.0: Pooled runtime: files of a store (G7).
 *
 * <p>Local layout: every shared root folder gets one sub-folder per store, {@code <root>/tenants/<tenantId>/}
 * ({@link #scopePath}); the images webapp keeps its existing layout {@code images/<tenantId>/} (catalog.properties
 * image.server.path). Object storage: the key of a file is {@code tenants/<tenantId>/<area>/<path>}
 * ({@link #key}). A JVM that writes a store file mirrors it into the storage ({@link #mirror}); a JVM that lacks
 * the file reads it from the storage ({@link #fetch}), so web and worker JVMs share files.</p>
 *
 * <p>Settings (general.properties): {@code tenant.storage.type} none (default), local or s3;
 * {@code tenant.storage.local.root}; {@code tenant.storage.s3.endpoint}, {@code .region}, {@code .bucket},
 * {@code .accessKey}, {@code .secretKey}.</p>
 */
public final class TenantFiles {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String RES = "general";
    private static final Pattern TENANT_ID = Pattern.compile("[A-Za-z0-9_-]{1,40}");
    private static final Pattern AREA = Pattern.compile("[a-z]{1,20}");

    public static final String AREA_IMAGES = "images";
    public static final String AREA_SITEMAPS = "sitemaps";

    /** Keys that the storage did not have, for a short time: a request for a missing file does not reach the storage each time */
    private static final java.util.Map<String, Long> misses = new java.util.concurrent.ConcurrentHashMap<>();
    private static final long MISS_TTL_MS = 10000L;

    private static volatile TenantStorage storage;
    private static volatile boolean storageLoaded;
    private static final ExecutorService uploader = new ThreadPoolExecutor(1, 4, 60, TimeUnit.SECONDS,
            new LinkedBlockingQueue<>(10000), r -> {
                Thread t = new Thread(r, "Scipio-TenantFilesUpload");
                t.setDaemon(true);
                return t;
            }, new ThreadPoolExecutor.CallerRunsPolicy());

    private TenantFiles() {}

    /** The configured storage, or null (tenant.storage.type=none, or not the pooled runtime). */
    public static TenantStorage getStorage() {
        if (!storageLoaded) {
            synchronized (TenantFiles.class) {
                if (!storageLoaded) {
                    storage = makeStorage();
                    storageLoaded = true;
                }
            }
        }
        return storage;
    }

    /** For tests: uses this storage instead of the configured one. */
    public static void setStorage(TenantStorage testStorage) {
        synchronized (TenantFiles.class) {
            storage = testStorage;
            storageLoaded = true;
        }
    }

    private static TenantStorage makeStorage() {
        if (!Tenants.isPooled()) {
            return null;
        }
        String type = UtilProperties.getPropertyValue(RES, "tenant.storage.type", "none");
        TenantStorage result = null;
        if ("local".equals(type)) {
            String root = UtilProperties.getPropertyValue(RES, "tenant.storage.local.root", "runtime/tenant-storage");
            Path path = Paths.get(root);
            if (!path.isAbsolute()) {
                path = Paths.get(System.getProperty("ofbiz.home", "."), root);
            }
            result = new LocalTenantStorage(path);
        } else if ("s3".equals(type)) {
            result = new S3TenantStorage(UtilProperties.getPropertyValue(RES, "tenant.storage.s3.endpoint"),
                    UtilProperties.getPropertyValue(RES, "tenant.storage.s3.region", "us-east-1"),
                    UtilProperties.getPropertyValue(RES, "tenant.storage.s3.bucket"),
                    UtilProperties.getPropertyValue(RES, "tenant.storage.s3.accessKey"),
                    UtilProperties.getPropertyValue(RES, "tenant.storage.s3.secretKey"));
        } else if (!"none".equals(type)) {
            Debug.logError("Tenant files: unknown tenant.storage.type [" + type + "]; no object storage", module);
        }
        if (result != null) {
            Debug.logInfo("Tenant files: object storage " + result.describe(), module);
        }
        return result;
    }

    /**
     * The storage key {@code tenants/<tenantId>/<area>/<relPath>}, or null when a part is not valid. The relative path
     * must not leave its area: no "..", no ".", no empty segment, no backslash, not absolute.
     */
    public static String key(String tenantId, String area, String relPath) {
        if (tenantId == null || !TENANT_ID.matcher(tenantId).matches() || area == null || !AREA.matcher(area).matches()
                || UtilValidate.isEmpty(relPath) || relPath.indexOf('\\') >= 0 || relPath.indexOf('\0') >= 0) {
            return null;
        }
        String rel = relPath.startsWith("/") ? relPath.substring(1) : relPath;
        for (String segment : rel.split("/", -1)) {
            if (segment.isEmpty() || ".".equals(segment) || "..".equals(segment)) {
                return null;
            }
        }
        return "tenants/" + tenantId + "/" + area + "/" + rel;
    }

    /**
     * The folder of the store below a shared root: {@code <basePath>/tenants/<tenantId>} for a store delegator in the
     * pooled runtime; basePath unchanged otherwise (base delegator, single-tenant).
     */
    public static String scopePath(String basePath, Delegator delegator) {
        String tenantId = (delegator != null) ? delegator.getDelegatorTenantId() : TenantScope.current();
        return scopePathOf(basePath, tenantId);
    }

    /** {@link #scopePath(String, Delegator)} for the store of the current thread (TenantScope). */
    public static String scopePath(String basePath) {
        return scopePathOf(basePath, TenantScope.current());
    }

    private static String scopePathOf(String basePath, String tenantId) {
        if (basePath == null || tenantId == null || !Tenants.isPooled() || !TENANT_ID.matcher(tenantId).matches()) {
            return basePath;
        }
        String base = basePath.endsWith("/") || basePath.endsWith("\\") ? basePath.substring(0, basePath.length() - 1) : basePath;
        return base + "/tenants/" + tenantId;
    }

    /** Uploads the local file of a store into the storage, in the background. No-op without storage. */
    public static void mirror(String tenantId, String area, String relPath, File file) {
        TenantStorage st = getStorage();
        String key = key(tenantId, area, relPath);
        if (st == null || key == null || file == null) {
            return;
        }
        Path path = file.toPath();
        uploader.execute(() -> {
            try {
                if (Files.isRegularFile(path)) {
                    st.put(key, path, contentType(path));
                    misses.remove(key);
                }
            } catch (IOException | RuntimeException e) {
                Debug.logError("Tenant files: could not store " + key + ": " + e.getMessage(), module);
            }
        });
    }

    /**
     * Uploads every file below a local folder of a store (for example a new sitemap) into the storage, in the
     * background; relPathPrefix is the path of the folder in the area.
     */
    public static void mirrorFolder(String tenantId, String area, String relPathPrefix, File folder) {
        if (getStorage() == null || folder == null || !folder.isDirectory()) {
            return;
        }
        File[] files = folder.listFiles();
        if (files == null) {
            return;
        }
        String prefix = UtilValidate.isEmpty(relPathPrefix) ? "" : (relPathPrefix.endsWith("/") ? relPathPrefix : relPathPrefix + "/");
        for (File f : files) {
            if (f.isFile()) {
                mirror(tenantId, area, prefix + f.getName(), f);
            }
        }
    }

    /**
     * Mirrors a file of the images webapp: a file below {@code <imagesRoot>/<tenantId>/} goes to
     * {@code tenants/<tenantId>/images/<rest>}. Returns false for any other file: a folder that is not a store (the
     * shared images of the base), or the folder of another store than the store of the thread.
     */
    public static boolean mirrorImage(File file) {
        if (getStorage() == null || file == null) {
            return false;
        }
        Path root = getImagesRoot();
        Path path = file.toPath().toAbsolutePath().normalize();
        if (!path.startsWith(root) || path.getNameCount() < root.getNameCount() + 2) {
            return false;
        }
        String tenantId = path.getName(root.getNameCount()).toString();
        String scope = TenantScope.current();
        // shared folders of the base (images/products/...) are no store; a store thread writes only its own folder
        if (!Tenants.exists(tenantId) || (scope != null && !scope.equals(tenantId))) {
            return false;
        }
        String rel = root.resolve(tenantId).relativize(path).toString().replace('\\', '/');
        mirror(tenantId, AREA_IMAGES, rel, file);
        return true;
    }

    /** The root folder of the images webapp (general.properties tenant.storage.imagesRoot). */
    public static Path getImagesRoot() {
        String root = UtilProperties.getPropertyValue(RES, "tenant.storage.imagesRoot", "framework/images/webapp/images");
        Path path = Paths.get(root);
        if (!path.isAbsolute()) {
            path = Paths.get(System.getProperty("ofbiz.home", "."), root);
        }
        return path.toAbsolutePath().normalize();
    }

    /**
     * Reads a store file from the storage into the local target (atomic move). Returns true when the file now exists
     * locally. The caller checked that the target is the right local place of this key.
     */
    public static boolean fetch(String tenantId, String area, String relPath, File target) {
        TenantStorage st = getStorage();
        String key = key(tenantId, area, relPath);
        if (st == null || key == null) {
            return false;
        }
        Long missUntil = misses.get(key);
        if (missUntil != null && missUntil > System.currentTimeMillis()) {
            return false;
        }
        try (InputStream in = st.get(key)) {
            if (in == null) {
                if (misses.size() >= 10000) {
                    misses.clear();
                }
                misses.put(key, System.currentTimeMillis() + MISS_TTL_MS);
                return false;
            }
            Path dest = target.toPath();
            Files.createDirectories(dest.getParent());
            Path tmp = Files.createTempFile(dest.getParent(), ".fetch", ".tmp");
            try {
                Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING);
                Files.move(tmp, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            } finally {
                Files.deleteIfExists(tmp);
            }
            return true;
        } catch (IOException | RuntimeException e) {
            Debug.logWarning("Tenant files: could not read " + key + ": " + e.getMessage(), module);
            return false;
        }
    }

    private static String contentType(Path path) {
        try {
            String type = Files.probeContentType(path);
            return (type != null) ? type : "application/octet-stream";
        } catch (IOException e) {
            return "application/octet-stream";
        }
    }
}
