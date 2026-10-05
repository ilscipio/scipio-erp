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

import org.ofbiz.base.util.GeneralException;

public interface FtpClientInterface {

    /**
     * Initialization of a file transfer client, and connection to the given server
     *
     * @param hostname hostname to connect to
     * @param username username to login with
     * @param password password to login with
     * @param port     port to connect to the server, optional
     * @param timeout  timeout for connection process, optional
     * @throws IOException
     */
    void connect(String hostname, String username, String password, Long port, Long timeout) throws IOException, GeneralException;

    /**
     * Copy of the give file to the connected server into the path.
     *
     * @param path     path to copy the file to
     * @param fileName name of the copied file
     * @param file     data to copy
     * @throws IOException
     */
    void copy(String path, String fileName, InputStream file) throws IOException;

    List<String> list(String path) throws IOException;

    void setBinaryTransfer(boolean isBinary) throws IOException;

    void setPassiveMode(boolean isPassive) throws IOException;

    /**
     * Close opened connection
     */
    void closeConnection() throws IOException;
}
