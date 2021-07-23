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
package org.ofbiz.entity;

import org.ofbiz.entity.jdbc.SQLProcessor;
import org.ofbiz.entity.model.ModelEntity;

/**
 * GenericDataSourceException.
 * <p>NOTE: The specific subtype of exception thrown - as determined by the {@link #from} factory method - depends
 * on the underlying database implementation types. If the calling code handles a subtype exception like
 * {@link EntityConstraintException}, it must handle the general error GenericDataSourceException as well to support
 * databases which still only return full exception types. Coverage should be improved.</p>
 * <p>SCIPIO: 2.1.0: Refactored to support specific source exceptions.</p>
 */
@SuppressWarnings("serial")
public class GenericDataSourceException extends GenericEntityException {

    public GenericDataSourceException() {
        super();
    }

    @Deprecated
    public GenericDataSourceException(String message) {
        super(message);
    }

    /**
     * Exception constructor - please use {@link #from}.
     * @see #from
     */
    public GenericDataSourceException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates a new exception of appropriate subtype where applicable, with support for null parameters.
     */
    public static GenericDataSourceException from(String message, Throwable cause, SQLProcessor sqlP,
                                                  Delegator delegator, ModelEntity modelEntity) {
        if (cause != null) {
            if (cause instanceof GenericDataSourceException) {
                // Wrapped exception - try to preserve exception type
                if (message == null || message.equals(cause.getMessage())) {
                    return (GenericDataSourceException) cause;
                }
                if (cause instanceof EntityForeignKeyException) {
                    return new EntityForeignKeyException(message, cause);
                } else if (cause instanceof EntityConstraintException) {
                    return new EntityConstraintException(message, cause);
                }
            } else {
                // SQL exception from database
                // TODO: Many more cases and refine existing
                String causeMsg = cause.getMessage();
                if (causeMsg != null) {
                    if (causeMsg.contains("caused a violation of foreign key constraint") ||
                            causeMsg.contains("violates foreign key constraint")) {
                        return new EntityForeignKeyException(message, cause);
                    }
                }
                if ("org.apache.derby.shared.common.error.DerbySQLIntegrityConstraintViolationException".equals(cause.getClass().getName())) {
                    return new EntityConstraintException(message, cause);
                }
            }
        }
        return new GenericDataSourceException(message, cause);
    }

    public static GenericDataSourceException getOriginal(Throwable t) {
        GenericDataSourceException gdse = null;
        while(t != null) {
            if (t instanceof GenericDataSourceException) {
                gdse = (GenericDataSourceException) t;
            }
            t = t.getCause();
        }
        return gdse;
    }
}
