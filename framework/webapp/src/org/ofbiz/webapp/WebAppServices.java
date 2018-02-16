/*******************************************************************************
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
 *******************************************************************************/
package org.ofbiz.webapp;

import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Map;

import javax.transaction.Transaction;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Generic services for webapp component.
 * Added 2018-02.
 */
public abstract class WebAppServices {

    public static final String module = WebAppServices.class.getName();
    
    private static final int VISIT_STAT_INTERVAL = 1000;
    
    protected WebAppServices() {
    }

    public static Map<String, Object> expireOldVisits(DispatchContext dctx, Map<String, ?> context) {
        OldVisitHandler handler = new OldVisitHandler(dctx, context, "expireOldVisits", "expire") {
            private Map<String, Object> fieldsToSet = UtilMisc.toMap("thruDate", now);
            
            @Override
            protected EntityCondition getVisitCond() {
                Timestamp olderThan = getVisitCutoffDate("serverstats", "stats.expire.visit.daysOld");

                Debug.logInfo(logPrefix+"Expiring all Visits older than: " + olderThan, module);
                
                return EntityCondition.makeCondition(
                        EntityCondition.makeCondition("thruDate", EntityOperator.EQUALS, null),
                        EntityOperator.AND,
                        EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, olderThan));
            }
            @Override
            protected void updateVisit(GenericValue visit) throws Exception {
                visit.set("thruDate", now);
                visit.store();
            }
            @Override
            protected int updateAllVisits(EntityCondition cond) throws Exception {
                return delegator.storeByCondition("Visit", fieldsToSet, cond);
            }
        };
        return handler.execAsService();
    }
    
    public static Map<String, Object> purgeOldVisits(DispatchContext dctx, Map<String, ?> context) {
        OldVisitHandler handler = new OldVisitHandler(dctx, context, "purgeOldVisits", "purge") {
            @Override
            protected EntityCondition getVisitCond() {
                Timestamp olderThan = getVisitCutoffDate("serverstats", "stats.purge.visit.daysOld");

                Debug.logInfo(logPrefix+"Removing all Visits older than: " + olderThan, module);

                return EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, olderThan);
            }
            @Override
            protected void updateVisit(GenericValue visit) throws Exception {
                visit.remove();
            }
            @Override
            protected int updateAllVisits(EntityCondition cond) throws Exception {
                return delegator.removeByCondition("Visit", cond);
            }
            
        };
        return handler.execAsService();
    }
    
    private static abstract class OldVisitHandler {
        
        protected final DispatchContext dctx;
        protected final Map<String, ?> context;
        protected final Delegator delegator;
        protected Timestamp now;
        protected final String serviceName;
        protected final String logPrefix;
        protected final String actionWord;
        
        public OldVisitHandler(DispatchContext dctx, Map<String, ?> context, String serviceName, String actionWord) {
            super();
            this.dctx = dctx;
            this.context = context;
            this.delegator = dctx.getDelegator();
            this.now = UtilDateTime.nowTimestamp();
            this.serviceName = serviceName;
            this.logPrefix = serviceName + ": ";
            this.actionWord = actionWord;
        }

        public Map<String, Object> execAsService() {
            if (Boolean.TRUE.equals(context.get("singleDbOp"))) {
                return execSingleOp();
            } else {
                return execPerRow();
            }
        }
        
        public Map<String, Object> execSingleOp() {
            boolean dryRun = Boolean.TRUE.equals(context.get("dryRun"));

            int success = 0;
            int error = 0;
            EntityCondition visitCond = getVisitCond();

            // SCIPIO: FIXME?: originally transaction handling was based on purgeOldJobs
            // but discovered some flaws, so forced to use a less efficient EntityListIterator usage
            // with constant transaction suspend/being/commit/resume...
            
            // always suspend the current transaction; use the one internally
            Transaction parent = null;
            try {
                if (TransactionUtil.getStatus() != TransactionUtil.STATUS_NO_TRANSACTION) {
                    parent = TransactionUtil.suspend();
                }

                boolean beganTx1 = false;

                try {
                    // begin this transaction
                    beganTx1 = TransactionUtil.begin();

                    if (!dryRun) {
                        success = updateAllVisits(visitCond);
                    }
                } catch (Exception e) {
                    Debug.logError(e, logPrefix+"Cannot " + actionWord + " visits", module);
                    try {
                        TransactionUtil.rollback(beganTx1, e.getMessage(), e);
                    } catch (GenericTransactionException e1) {
                        Debug.logWarning(e1, module);
                    }
                    return ServiceUtil.returnError(e.getMessage());
                } finally {
                    try {
                        TransactionUtil.commit(beganTx1);
                    } catch (GenericTransactionException e) {
                        Debug.logError(e, logPrefix+"Cannot commit " + actionWord + " visits", module);
                        error = success;
                        success = 0;
                    }
                }

            } catch (GenericTransactionException e) {
                Debug.logError(e, logPrefix+"Unable to suspend transaction; cannot " + actionWord + " Visits!", module);
                return ServiceUtil.returnError(e.getMessage());
            } finally {
                if (parent != null) {
                    try {
                        TransactionUtil.resume(parent);
                    } catch (GenericTransactionException e) {
                        Debug.logWarning(e, module);
                    }
                }
            }

            if (error > 0) {
                String msg = success + " visits " + actionWord + "d; " + error + " failed";
                Debug.logWarning(logPrefix+msg, module);
                return ServiceUtil.returnFailure(msg);
            } else {
                String msg = success + " visits " + actionWord + "d";
                Debug.logInfo(logPrefix+msg, module);
                return ServiceUtil.returnSuccess(msg);
            }
        }
        
        
        public Map<String, Object> execPerRow() {
            boolean dryRun = Boolean.TRUE.equals(context.get("dryRun"));
            
            int success = 0;
            int error = 0;
            int visited = 0;
            int total = 0;
            EntityCondition visitCond = getVisitCond();

            // SCIPIO: FIXME?: originally transaction handling was based on purgeOldJobs
            // but discovered some flaws, so forced to use a less efficient EntityListIterator usage
            // with constant transaction suspend/being/commit/resume...
            
            // always suspend the current transaction; use the one internally
            Transaction parent = null;
            try {
                if (TransactionUtil.getStatus() != TransactionUtil.STATUS_NO_TRANSACTION) {
                    parent = TransactionUtil.suspend();
                }

                boolean beganTx1 = false;

                try {
                    // begin this transaction
                    beganTx1 = TransactionUtil.begin();

                    EntityListIterator foundVisits = null;
                    try {
                        foundVisits = EntityQuery.use(delegator)
                                               .from("Visit")
                                               .where(visitCond)
                                               .cursorScrollInsensitive()
                                               //.maxRows(1000) // from purgeOldJobs, can't do this now...
                                               .queryIterator();
                        total = foundVisits.getResultsSizeAfterPartialList();
                        GenericValue visit;
                        while((visit = foundVisits.next()) != null) {
                            boolean isSuccess = false;

                            Transaction parentTx1 = null;
                            boolean beganTx2 = false;
                            try {
                                parentTx1 = TransactionUtil.suspend();
                                beganTx2 = TransactionUtil.begin();
                                try {
                                    if (!dryRun) {
                                        updateVisit(visit);
                                    }
                                    isSuccess = true;
                                } catch (Exception e) {
                                    Debug.logError(logPrefix+"Error " + actionWord + " Visit '" + visit.get("visitId") + ": " + e.getMessage(), module);
                                    try {
                                        TransactionUtil.rollback(beganTx2, e.getMessage(), e);
                                    } catch (GenericTransactionException e1) {
                                        Debug.logWarning(e1, module);
                                    }
                                }
                            } catch (Exception e) {
                                // FATAL
                                Debug.logError(logPrefix+"Error preparing transaction for Visit '" + visit.get("visitId"), module);
                                return ServiceUtil.returnError("Error preparing transaction for Visit '" + visit.get("visitId"));
                            } finally {
                                try {
                                    TransactionUtil.commit(beganTx2);
                                } catch (GenericTransactionException e) {
                                    Debug.logError(e, logPrefix+"Error committing " + actionWord + " for Visit '" + visit.get("visitId") + ": " + e.getMessage(), module);
                                    isSuccess = false;
                                }
                                
                                if (parentTx1 != null) {
                                    try {
                                        TransactionUtil.resume(parentTx1);
                                    } catch (GenericTransactionException ise) {
                                        Debug.logError(ise, logPrefix+"Error resuming parent transaction " + " for Visit '" + visit.get("visitId"), module);
                                        // FATAL
                                        return ServiceUtil.returnError("Error resuming parent transaction " + " for Visit '" + visit.get("visitId"));
                                    }
                                }
                            }
                            
                            if (isSuccess) success++;
                            else error++;
                            visited++;
                            
                            if ((visited % VISIT_STAT_INTERVAL) == 0) {
                                Debug.logInfo(logPrefix+"Processed " + visited + "/" + total 
                                        + " visits (success: " + success + ", error: " + error + ")", module);
                            }
                        }
                    } finally {
                        if (foundVisits != null) {
                            foundVisits.close();
                        }
                    }
                } catch (GenericEntityException e) {
                    Debug.logError(e, logPrefix+"Cannot obtain Visit from datasource", module);
                    try {
                        TransactionUtil.rollback(beganTx1, e.getMessage(), e);
                    } catch (GenericTransactionException e1) {
                        Debug.logWarning(e1, module);
                    }
                    return ServiceUtil.returnError(e.getMessage());
                } finally {
                    try {
                        TransactionUtil.commit(beganTx1);
                    } catch (GenericTransactionException e) {
                        Debug.logWarning(e, module);
                    }
                }

            } catch (GenericTransactionException e) {
                Debug.logError(e, logPrefix+"Unable to suspend transaction; cannot " + actionWord + " Visits!", module);
                return ServiceUtil.returnError(e.getMessage());
            } finally {
                if (parent != null) {
                    try {
                        TransactionUtil.resume(parent);
                    } catch (GenericTransactionException e) {
                        Debug.logWarning(e, module);
                    }
                }
            }

            if (error > 0) {
                String msg = success + " visits " + actionWord + "d; " + error + " failed";
                Debug.logWarning(logPrefix+msg, module);
                return ServiceUtil.returnFailure(msg);
            } else {
                String msg = success + " visits " + actionWord + "d";
                Debug.logInfo(logPrefix+msg, module);
                return ServiceUtil.returnSuccess(msg);
            }
        }
        
        protected abstract EntityCondition getVisitCond();
        
        protected abstract void updateVisit(GenericValue visit) throws Exception;
        
        protected abstract int updateAllVisits(EntityCondition cond) throws Exception;
        
        protected final Timestamp getVisitCutoffDate(String propResource, String propName) {
            Timestamp olderThan = (Timestamp) context.get("olderThan");
            if (olderThan == null) {
                Integer daysOld = (Integer) context.get("daysOld");
                if (daysOld == null) {
                    daysOld = UtilProperties.getPropertyAsInteger(propResource, propName, 30);
                }
                olderThan = UtilDateTime.adjustTimestamp(now, Calendar.DAY_OF_MONTH, -daysOld);
            }
            return olderThan;
        }
    }

}
