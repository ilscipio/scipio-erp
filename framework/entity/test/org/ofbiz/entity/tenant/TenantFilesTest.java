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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * SCIPIO: 4.0.0: Pooled runtime isolation rules of store files (G7, G19): a key never leaves the store and the area.
 */
public class TenantFilesTest {

    @Test
    public void keyHasStoreAndArea() {
        assertEquals("tenants/t001/images/products/SPIKE-MARK/original.png", TenantFiles.key("t001", "images", "products/SPIKE-MARK/original.png"));
        assertEquals("tenants/t001/sitemaps/sitemap_index.xml", TenantFiles.key("t001", "sitemaps", "/sitemap_index.xml"));
    }

    @Test
    public void keyRefusesTraversalAndBadParts() {
        assertNull(TenantFiles.key("t001", "images", "../t002/images/x.png"));
        assertNull(TenantFiles.key("t001", "images", "products/../../t002/x.png"));
        assertNull(TenantFiles.key("t001", "images", "products/./x.png"));
        assertNull(TenantFiles.key("t001", "images", "products//x.png"));
        assertNull(TenantFiles.key("t001", "images", "products\\x.png"));
        assertNull(TenantFiles.key("t001", "images", ""));
        assertNull(TenantFiles.key("t001", "images", "products/"));
        assertNull(TenantFiles.key("t/001", "images", "x.png"));
        assertNull(TenantFiles.key("..", "images", "x.png"));
        assertNull(TenantFiles.key(null, "images", "x.png"));
        assertNull(TenantFiles.key("t001", "../x", "x.png"));
        assertNull(TenantFiles.key("t001", "Images", "x.png"));
    }

    @Test
    public void localStorageRoundTripAndRoot(@TempDir Path dir) throws IOException {
        LocalTenantStorage storage = new LocalTenantStorage(dir.resolve("store"));
        Path src = dir.resolve("src.txt");
        Files.write(src, "marker".getBytes(StandardCharsets.UTF_8));
        String key = TenantFiles.key("t001", "images", "a/b.txt");
        storage.put(key, src, "text/plain");
        try (InputStream in = storage.get(key)) {
            assertEquals("marker", new String(in.readAllBytes(), StandardCharsets.UTF_8));
        }
        assertNull(storage.get(TenantFiles.key("t002", "images", "a/b.txt")));
        storage.delete(key);
        assertNull(storage.get(key));
        assertThrows(IOException.class, () -> storage.get("../outside.txt"));
        assertThrows(IOException.class, () -> storage.put("../outside.txt", src, "text/plain"));
    }

    @Test
    public void s3PathSegmentEncoding() {
        assertEquals("a%20b%2Bc~-_.", S3TenantStorage.encode("a b+c~-_."));
        assertEquals("%C3%A4", S3TenantStorage.encode("ä"));
    }

    /** Runs only with an S3 endpoint (for example MinIO): SCIPIO_TEST_S3_ENDPOINT, _BUCKET, _ACCESS_KEY, _SECRET_KEY. */
    @Test
    public void s3RoundTrip(@TempDir Path dir) throws IOException {
        String endpoint = System.getenv("SCIPIO_TEST_S3_ENDPOINT");
        Assumptions.assumeTrue(endpoint != null && !endpoint.isEmpty(), "no S3 endpoint");
        S3TenantStorage storage = new S3TenantStorage(endpoint, "us-east-1", System.getenv("SCIPIO_TEST_S3_BUCKET"),
                System.getenv("SCIPIO_TEST_S3_ACCESS_KEY"), System.getenv("SCIPIO_TEST_S3_SECRET_KEY"));
        storage.createBucket();
        Path src = dir.resolve("src.txt");
        Files.write(src, "s3 marker".getBytes(StandardCharsets.UTF_8));
        String key = TenantFiles.key("t001", "images", "products/X 1/a+b.txt");
        storage.put(key, src, "text/plain");
        try (InputStream in = storage.get(key)) {
            assertEquals("s3 marker", new String(in.readAllBytes(), StandardCharsets.UTF_8));
        }
        assertNull(storage.get(TenantFiles.key("t002", "images", "products/X 1/a+b.txt")));
        storage.delete(key);
        assertNull(storage.get(key));
        assertTrue(storage.describe().startsWith("s3:"));
    }
}
