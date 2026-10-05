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
package org.ofbiz.content.ftp;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class SecureFtpClient implements FtpClientInterface {

    public static final String module = SecureFtpClient.class.getName();

    /**
     * TODO : to implements
     */
    @Override
    public void connect(String hostname, String username, String password, Long port, Long timeout) throws IOException {

    }

    @Override
    public void copy(String path, String fileName, InputStream file) throws IOException {

    }

    @Override
    public List<String> list(String path) throws IOException {
        return null;
    }

    @Override
    public void setBinaryTransfer(boolean isBinary) throws IOException {

    }

    @Override
    public void setPassiveMode(boolean isPassive) throws IOException {

    }

    @Override
    public void closeConnection() {

    }
}
