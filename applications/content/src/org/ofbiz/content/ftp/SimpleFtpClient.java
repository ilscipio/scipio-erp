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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPReply;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilProperties;

public class SimpleFtpClient implements FtpClientInterface {

    public static final String module = SimpleFtpClient.class.getName();
    private FTPClient client;

    public SimpleFtpClient() {
        client = new FTPClient();
    }

    @Override
    public void connect(String hostname, String username, String password, Long port, Long timeout) throws IOException, GeneralException {
        if (client == null) return;
        if (client.isConnected()) return;
        if (port != null) {
            client.connect(hostname, port.intValue());
        } else {
            client.connect(hostname);
        }
        if (timeout != null) client.setDefaultTimeout(timeout.intValue());

        if (!FTPReply.isPositiveCompletion(client.getReplyCode())) {
            Debug.logError("Server refused connection", module);
            throw new GeneralException(UtilProperties.getMessage("CommonUiLabels", "CommonFtpConnectionRefused", Locale.getDefault()));
        }
        if (!client.login(username, password)) {
            Debug.logError("login failed", module);
            throw new GeneralException(UtilProperties.getMessage("CommonUiLabels", "CommonFtpLoginFailure", Locale.getDefault()));
        }
    }

    @Override
    public List<String> list(String path) throws IOException {
        FTPFile[] files = client.listFiles(path);
        List<String> fileNames = new ArrayList<>();
        for (FTPFile file : files) {
            fileNames.add(file.getName());
        }
        return fileNames;
    }

    public void setBinaryTransfer(boolean isBinary) throws IOException {
        if (isBinary) {
            client.setFileType(FTP.BINARY_FILE_TYPE);
        } else {
            client.setFileType(FTP.ASCII_FILE_TYPE);
        }
    }

    public void setPassiveMode(boolean isPassive) {
        if (isPassive) {
            client.enterLocalPassiveMode();
        } else {
            client.enterLocalActiveMode();
        }
    }

    @Override
    public void copy(String path, String fileName, InputStream file) throws IOException {
        client.changeWorkingDirectory(path);
        client.storeFile(fileName, file);
    }

    @Override
    public void closeConnection() throws IOException {
        if (client != null && client.isConnected()) {
            client.logout();
        }
    }
}
