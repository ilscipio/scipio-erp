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
package com.ilscipio.scipio.service.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Test_seServices {

    @Service(
        name = "testServiceDeadLockRetry",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceDeadLockRetry",
        implemented = {@Implements(service = "testServiceInterface")}
    )
    public interface TestServiceDeadLockRetry {}

    @Service(
        name = "testServiceDeadLockRetryThreadA",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceDeadLockRetryThreadA"
    )
    public interface TestServiceDeadLockRetryThreadA {}

    @Service(
        name = "testServiceDeadLockRetryThreadB",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceDeadLockRetryThreadB"
    )
    public interface TestServiceDeadLockRetryThreadB {}

    @Service(
        name = "testServiceLockWaitTimeoutRetry",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceLockWaitTimeoutRetry",
        implemented = {@Implements(service = "testServiceInterface")}
    )
    public interface TestServiceLockWaitTimeoutRetry {}

    @Service(
        name = "testServiceLockWaitTimeoutRetryGrabber",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceLockWaitTimeoutRetryGrabber",
        transactionTimeout = "6"
    )
    public interface TestServiceLockWaitTimeoutRetryGrabber {}

    @Service(
        name = "testServiceLockWaitTimeoutRetryWaiter",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceLockWaitTimeoutRetryWaiter",
        transactionTimeout = "2"
    )
    public interface TestServiceLockWaitTimeoutRetryWaiter {}

    @Service(
        name = "testEntityAutoCreateTestingPkPresent",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "Testing",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface TestEntityAutoCreateTestingPkPresent {}

    @Service(
        name = "testEntityAutoCreateTestingPkMissing",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "Testing",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "testingId", type = "String", mode = "OUT")
        }
    )
    public interface TestEntityAutoCreateTestingPkMissing {}

    @Service(
        name = "testEntityAutoCreateTestingItemPkPresent",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "TestingItem",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface TestEntityAutoCreateTestingItemPkPresent {}

    @Service(
        name = "testEntityAutoCreateTestingItemPkMissing",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "TestingItem",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "testingId", type = "String", mode = "IN"),
            @Attribute(name = "testingSeqId", type = "String", mode = "OUT")
        }
    )
    public interface TestEntityAutoCreateTestingItemPkMissing {}

    @Service(
        name = "testEntityAutoCreateTestingNodeMemberPkPresent",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "TestingNodeMember",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface TestEntityAutoCreateTestingNodeMemberPkPresent {}

    @Service(
        name = "testEntityAutoCreateTestingNodeMemberPkMissing",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "TestingNodeMember",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "testingId", type = "String", mode = "IN"),
            @Attribute(name = "testingNodeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT")
        }
    )
    public interface TestEntityAutoCreateTestingNodeMemberPkMissing {}

    @Service(
        name = "testEntityAutoUpdateTesting",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "Testing",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface TestEntityAutoUpdateTesting {}

    @Service(
        name = "testEntityAutoRemoveTesting",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "Testing",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface TestEntityAutoRemoveTesting {}

    @Service(
        name = "testEntityAutoExpireTestingNodeMember",
        engine = "entity-auto",
        invoke = "expire",
        defaultEntityName = "TestingNodeMember",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface TestEntityAutoExpireTestingNodeMember {}

    @Service(
        name = "testEntityAutoExpireTestFieldType",
        engine = "entity-auto",
        invoke = "expire",
        defaultEntityName = "TestFieldType",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "dateTimeField", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface TestEntityAutoExpireTestFieldType {}

    @Service(
        name = "testEntityAutoCreateTestingStatus",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "TestingStatus",
        attributes = {
            @Attribute(name = "testingId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "testingStatusId", type = "String", mode = "OUT")
        }
    )
    public interface TestEntityAutoCreateTestingStatus {}

    @Service(
        name = "testEntityAutoUpdateTestingStatus",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "TestingStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN")
        }
    )
    public interface TestEntityAutoUpdateTestingStatus {}

    @Service(
        name = "testEntityAutoDeleteTestingStatus",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "TestingStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface TestEntityAutoDeleteTestingStatus {}

    @Service(
        name = "testServiceLockWaitTimeoutRetryCantRecover",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceLockWaitTimeoutRetryCantRecover",
        transactionTimeout = "2",
        implemented = {@Implements(service = "testServiceInterface")}
    )
    public interface TestServiceLockWaitTimeoutRetryCantRecover {}

    @Service(
        name = "testServiceLockWaitTimeoutRetryCantRecoverWaiter",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceLockWaitTimeoutRetryCantRecoverWaiter",
        transactionTimeout = "4"
    )
    public interface TestServiceLockWaitTimeoutRetryCantRecoverWaiter {}

    @Service(
        name = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParentErrorCatchWrapper",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParentErrorCatchWrapper",
        implemented = {@Implements(service = "testServiceInterface")}
    )
    public interface TestServiceOwnTxSubServiceAfterSetRollbackOnlyInParentErrorCatchWrapper {}

    @Service(
        name = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParent",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParent"
    )
    public interface TestServiceOwnTxSubServiceAfterSetRollbackOnlyInParent {}

    @Service(
        name = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParentSubService",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceOwnTxSubServiceAfterSetRollbackOnlyInParentSubService",
        requireNewTransaction = "true"
    )
    public interface TestServiceOwnTxSubServiceAfterSetRollbackOnlyInParentSubService {}

    @Service(
        name = "testServiceEcaGlobalEventExec",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceEcaGlobalEventExec",
        implemented = {@Implements(service = "testServiceInterface")}
    )
    public interface TestServiceEcaGlobalEventExec {}

    @Service(
        name = "testServiceEcaGlobalEventExecOnCommit",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceEcaGlobalEventExecOnCommit"
    )
    public interface TestServiceEcaGlobalEventExecOnCommit {}

    @Service(
        name = "testServiceEcaGlobalEventExecToRollback",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceEcaGlobalEventExecToRollback"
    )
    public interface TestServiceEcaGlobalEventExecToRollback {}

    @Service(
        name = "testServiceEcaGlobalEventExecOnRollback",
        location = "org.ofbiz.service.test.ServiceEngineTestServices",
        invoke = "testServiceEcaGlobalEventExecOnRollback"
    )
    public interface TestServiceEcaGlobalEventExecOnRollback {}

}
