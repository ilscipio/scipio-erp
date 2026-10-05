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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.ofbiz.base.util.UtilValidate;
import org.apache.sshd.client.SshClient;
import org.apache.sshd.client.session.ClientSession;
import org.apache.sshd.client.subsystem.sftp.SftpClient;
import org.apache.sshd.client.subsystem.sftp.SftpClientFactory;

/**
 * Basic client to copy files to an ssh ftp server
 */
public class SshFtpClient implements FtpClientInterface {

    public static final String module = SshFtpClient.class.getName();

    private SshClient client;
    private SftpClient sftp;

    public SshFtpClient() {
        client = SshClient.setUpDefaultClient();
        client.start();
    }

    @Override
    public void connect(String hostname, String username, String password, Long port, Long timeout) throws IOException {
        if (port == null) port = 22L;
        if (timeout == null) timeout = 10000L;

        if (sftp != null) return;
        ClientSession session = client.connect(username, hostname, port.intValue()).verify(timeout.intValue()).getSession();
        session.addPasswordIdentity(password);
        session.auth().verify(timeout.intValue());
        // SCIPIO: 2018-09-10: changed for sshd-sftp 2.0.0
        //sftp = session.createSftpClient();
        sftp = SftpClientFactory.instance().createSftpClient(session);
    }

    @Override
    public void copy(String path, String fileName, InputStream file) throws IOException {
        OutputStream os = sftp.write((UtilValidate.isNotEmpty(path) ? path + "/" : "") + fileName);
        IOUtils.copy(file, os);
        os.close();
    }

    @Override
    public List<String> list(String path) throws IOException {
        SftpClient.CloseableHandle handle = sftp.openDir((UtilValidate.isNotEmpty(path) ? path + "/" : ""));
        List<String> fileNames = new ArrayList<>();
        for (SftpClient.DirEntry dirEntry : sftp.listDir(handle)) {
            fileNames.add(dirEntry.getFilename());
        }
        return fileNames;
    }

    @Override
    public void setBinaryTransfer(boolean isBinary) throws IOException {
    }

    @Override
    public void setPassiveMode(boolean isPassive) throws IOException {
    }

    @Override
    public void closeConnection() {
        if (sftp != null) {
            client.stop();
            sftp = null;
        }
    }
}
