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
package org.ofbiz.service;

import java.util.Collection;
import java.util.Map;

/**
 * SCIPIO: Used in conjunction with {@link ServiceSynchronization} to allow client code
 * to query some (limited) info about the service executions registered in the current
 * transaction.
 * Added 2017-12-20.
 * @see LocalDispatcher#getServiceSyncRegistrations()
 * @see ServiceSynchronization
 */
public interface ServiceSyncRegistrations {

    Collection<ServiceSyncRegistration> getAllRegistrations();

    Collection<ServiceSyncRegistration> getRegistrationsForService(String serviceName);
    Collection<ServiceSyncRegistration> getCommitRegistrationsForService(String serviceName);
    Collection<ServiceSyncRegistration> getRollbackRegistrationsForService(String serviceName);

    ServiceSyncRegistration getFirstRegistrationForService(String serviceName);
    ServiceSyncRegistration getFirstCommitRegistrationForService(String serviceName);
    ServiceSyncRegistration getFirstRollbackRegistrationForService(String serviceName);

    void addCommitService(DispatchContext dctx, String serviceName, String runAsUser, Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException;
    void addRollbackService(DispatchContext dctx, String serviceName, String runAsUser, Map<String, ? extends Object> context, boolean async, boolean persist) throws GenericServiceException;

    boolean removeService(ServiceSyncRegistration serviceRegistration) throws GenericServiceException;
    int removeService(String serviceName) throws GenericServiceException;
    int removeCommitService(String serviceName) throws GenericServiceException;
    int removeRollbackService(String serviceName) throws GenericServiceException;

    public interface ServiceSyncRegistration {
        String getServiceName();
        boolean isCommit();
        boolean isRollback();
        /**
         * Returns the service context.
         * WARN: Use the returned map with care - it should NOT be modified in-place,
         * but this may not necessarily be enforced.
         */
        Map<String, ?> getContext();
        boolean isAsync();
        boolean isPersist();
    }
}
