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
package com.ilscipio.scipio.workeffort.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    /**
     *              Migrate data from OldWorkEffortContactMech to WorkEffortContactMech.             Since revision 827903 (2009-10-21) the entity OldWorkEffortContactMech has been deprecated.             This service can be used to upgrade existing data from the OldWorkEffortContactMech entity to the new             WorkEffortContactMech entity.         
     */
    @Service(
        name = "migrateWorkEffortContactMech",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/workeffort/UpgradeServices.xml",
        invoke = "migrateWorkEffortContactMech",
        description = "\n            Migrate data from OldWorkEffortContactMech to WorkEffortContactMech.\n            Since revision 827903 (2009-10-21) the entity OldWorkEffortContactMech has been deprecated.\n            This service can be used to upgrade existing data from the OldWorkEffortContactMech entity to the new\n            WorkEffortContactMech entity.\n        "
    )
    public interface MigrateWorkEffortContactMech {}

}
