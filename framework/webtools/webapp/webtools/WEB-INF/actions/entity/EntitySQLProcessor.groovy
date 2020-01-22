/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */


import org.ofbiz.base.util.Debug
import org.ofbiz.entity.jdbc.SQLProcessor
import org.ofbiz.entity.transaction.GenericTransactionException
import org.ofbiz.entity.transaction.TransactionUtil

import javax.transaction.Transaction;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import org.ofbiz.entity.*;
import org.ofbiz.entity.model.ModelGroupReader;

def module = "EntitySQLProcessor.groovy";

sqlCommand = context.request.getParameter("sqlCommand");
if (sqlCommand) {
    sqlCommand = sqlCommand.trim();
}

resultMessage = "";
rs = null;
columns = [];
records = [];
mgr = delegator.getModelGroupReader();
groups = mgr.getGroupNames(delegator.getDelegatorName());

if (sqlCommand && selGroup) {
    // SCIPIO: Fixed transaction breaking screen
    Transaction suspendedTransaction = null;
    try {
        if (TransactionUtil.isTransactionInPlace()) { // SCIPIO: 2018-09-04: added check to eliminate useless warnings
            suspendedTransaction = TransactionUtil.suspend();
        }
        beganTransaction = false;
        try {
            beganTransaction = TransactionUtil.begin();
            du = new SQLProcessor(delegator, delegator.getGroupHelperInfo(selGroup));
            if (sqlCommand.toUpperCase().startsWith("SELECT")) {
                rs = du.executeQuery(sqlCommand);
                if (rs != null) {
                    try {
                        rsmd = rs.getMetaData();
                        numberOfColumns = rsmd.getColumnCount();
                        for (i = 1; i <= numberOfColumns; i++) {
                            columns.add(rsmd.getColumnName(i));
                        }
                        rowLimitReached = false;
                        while (rs.next()) {
                            if (records.size() >= rowLimit) {
                                resultMessage = "Returned top $rowLimit rows.";
                                rowLimitReached = true;
                                break;
                            }
                            record = [];
                            for (i = 1; i <= numberOfColumns; i++) {
                                record.add(rs.getObject(i));
                            }
                            records.add(record);
                        }
                        resultMessage = "Returned " + (rowLimitReached ? "top " + rowLimit : "" + records.size()) + " rows.";
                    } finally { // SCIPIO: Fixed missing finally
                        rs.close();
                    }
                }
            } else {
                du.prepareStatement(sqlCommand);
                numOfAffectedRows = du.executeUpdate();
                resultMessage = "Affected $numOfAffectedRows rows.";
            }
            TransactionUtil.commit(beganTransaction);
        } catch (Exception e) {
            resultMessage = e.getMessage();
            errMsg = "SQL command error for command: " + sqlCommand + ": " + resultMessage;
            Debug.logError(e, errMsg, module);
            try {
                TransactionUtil.rollback(beganTransaction, errMsg, e);
            } catch (GenericTransactionException e2) {
                Debug.logError(e2, "Unable to rollback transaction", module);
            }
        }
    } finally {
        if (suspendedTransaction != null) {
            try {
                TransactionUtil.resume(suspendedTransaction);
            } catch (GenericTransactionException e) {
                Debug.logError(e, "Error resuming suspended transaction", module);
            }
        }
    }
}
context.groups = groups;
context.resultMessage = resultMessage;
context.columns = columns;
context.records = records;
context.sqlCommand = sqlCommand; // (see OFBIZ-6567)
