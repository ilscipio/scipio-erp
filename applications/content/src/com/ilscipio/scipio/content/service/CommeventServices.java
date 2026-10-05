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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommeventServices {

    /**
     * Create CommunicationEvent and Content
     */
    @Service(
        name = "createCommContentDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createCommContentDataResource",
        description = "Create CommunicationEvent and Content",
        auth = "true",
        implemented = {@Implements(service = "persistContentAndAssoc")},
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "OUT")
        }
    )
    public interface CreateCommContentDataResource {}

    /**
     * Update CommunicationEvent and Content
     */
    @Service(
        name = "updateCommContentDataResource",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateCommContentDataResource",
        description = "Update CommunicationEvent and Content",
        auth = "true",
        implemented = {@Implements(service = "persistContentAndAssoc")},
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN")
        }
    )
    public interface UpdateCommContentDataResource {}

    /**
     * Create CommEventContentAssoc
     */
    @Service(
        name = "createCommEventContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "createCommEventContentAssoc",
        description = "Create CommEventContentAssoc",
        auth = "true",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "INOUT", optional = "true"),
            @Attribute(name = "thruDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface CreateCommEventContentAssoc {}

    /**
     * Update CommEventContentAssoc
     */
    @Service(
        name = "updateCommEventContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "updateCommEventContentAssoc",
        description = "Update CommEventContentAssoc",
        auth = "true",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        }
    )
    public interface UpdateCommEventContentAssoc {}

    /**
     * Expire CommEventContentAssoc
     */
    @Service(
        name = "expireCommEventContentAssoc",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire CommEventContentAssoc",
        defaultEntityName = "CommEventContentAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireCommEventContentAssoc {}

    /**
     * Remove CommEventContentAssoc
     */
    @Service(
        name = "removeCommEventContentAssoc",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/content/ContentServices.xml",
        invoke = "removeCommEventContentAssoc",
        description = "Remove CommEventContentAssoc",
        auth = "true",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "java.sql.Timestamp", mode = "IN")
        }
    )
    public interface RemoveCommEventContentAssoc {}

    /**
     * Create a new Comm Content Assoc Type Record
     */
    @Service(
        name = "createCommContentAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Comm Content Assoc Type Record",
        defaultEntityName = "CommContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCommContentAssocType {}

    /**
     * Update a Comm Content Assoc Type
     */
    @Service(
        name = "updateCommContentAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Comm Content Assoc Type",
        defaultEntityName = "CommContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCommContentAssocType {}

    /**
     * Delete an existing Comm Content Assoc Type Record
     */
    @Service(
        name = "deleteCommContentAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Comm Content Assoc Type Record",
        defaultEntityName = "CommContentAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCommContentAssocType {}

}
