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

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * SCIPIO: 4.0.0: Pooled runtime: {@link TenantStorage} in a local (or mounted) folder: the key is the relative path
 * below the root. For one-machine setups and tests; pods use {@link S3TenantStorage}.
 */
public class LocalTenantStorage implements TenantStorage {

    private final Path root;

    public LocalTenantStorage(Path root) {
        this.root = root.toAbsolutePath().normalize();
    }

    private Path resolve(String key) throws IOException {
        Path path = root.resolve(key).normalize();
        if (!path.startsWith(root) || path.equals(root)) {
            throw new IOException("Key outside the storage root: " + key);
        }
        return path;
    }

    @Override
    public void put(String key, Path file, String contentType) throws IOException {
        Path target = resolve(key);
        Files.createDirectories(target.getParent());
        Path tmp = Files.createTempFile(target.getParent(), ".put", ".tmp");
        try {
            Files.copy(file, tmp, StandardCopyOption.REPLACE_EXISTING);
            Files.move(tmp, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } finally {
            Files.deleteIfExists(tmp);
        }
    }

    @Override
    public InputStream get(String key) throws IOException {
        try {
            return Files.newInputStream(resolve(key));
        } catch (NoSuchFileException e) {
            return null;
        }
    }

    @Override
    public void delete(String key) throws IOException {
        Files.deleteIfExists(resolve(key));
    }

    @Override
    public String describe() {
        return "local:" + root;
    }
}
