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
package org.ofbiz.entity.transaction;

import java.sql.Connection;
import java.sql.SQLException;

import javax.transaction.TransactionManager;
import javax.transaction.UserTransaction;
import javax.transaction.xa.XAException;

import org.apache.geronimo.transaction.log.UnrecoverableLog;
import org.apache.geronimo.transaction.manager.GeronimoTransactionManager;
import org.apache.geronimo.transaction.manager.TransactionLog;
import org.apache.geronimo.transaction.manager.XidFactoryImpl;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.config.model.Datasource;
import org.ofbiz.entity.config.model.EntityConfig;
import org.ofbiz.entity.datasource.GenericHelperInfo;
import org.ofbiz.entity.jdbc.ConnectionFactoryLoader;

public class GeronimoTransactionFactory implements TransactionFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static int defaultTransactionTimeoutSeconds = 60;
    private static TransactionLog transactionLog;
    private static GeronimoTransactionManager geronimoTransactionManager;

    static {
        // creates an instance of Geronimo transaction context, etc with a local transaction factory which is not bound to a registry
        try {
            transactionLog = new UnrecoverableLog();
            geronimoTransactionManager = new GeronimoTransactionManager(defaultTransactionTimeoutSeconds, new XidFactoryImpl(), transactionLog);
        } catch (XAException e) {
            Debug.logError(e, "Error initializing Geronimo transaction manager: " + e.toString(), module);
        }
    }

    /*
     * @see org.ofbiz.entity.transaction.TransactionFactory#getTransactionManager()
     */
    public TransactionManager getTransactionManager() {
        return geronimoTransactionManager;
    }

    /*
     * @see org.ofbiz.entity.transaction.TransactionFactory#getUserTransaction()
     */
    public UserTransaction getUserTransaction() {
        return geronimoTransactionManager;
    }

    public String getTxMgrName() {
        return "geronimo";
    }

    public Connection getConnection(GenericHelperInfo helperInfo) throws SQLException, GenericEntityException {
        Datasource datasourceInfo = EntityConfig.getDatasource(helperInfo.getHelperBaseName());

        if (datasourceInfo != null && datasourceInfo.getInlineJdbc() != null) {
            return ConnectionFactoryLoader.getInstance().getConnection(helperInfo, datasourceInfo.getInlineJdbc());
        }
        Debug.logError("Geronimo is the configured transaction manager but no inline-jdbc element was specified in the " + helperInfo.getHelperBaseName() + " datasource. Please check your configuration", module);
        return null;
    }

    public void shutdown() {
        ConnectionFactoryLoader.getInstance().closeAll();
    }
}
