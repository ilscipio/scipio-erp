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
/**
 * Master HTML templating variables include, Foundation Shop theme/framework.
 *
 * Overrides the default SCIPIO styles located in
 * htmlVariables.groovy - ofbiz_foundation/framework/common/webcommon/includes/scipio/lib/standard/
 *
 */

import java.lang.*;
import java.util.*;
import org.ofbiz.base.util.*;
 
/*
*************************************
* MAIN STYLES *
*************************************
*/

/* EXTENDS/MODIFIES the default foundation styles */
GroovyUtil.runScriptAtLocation("component://common/webcommon/includes/scipio/lib/standard/htmlVariables.groovy", null, context);

context.styles.putAll([
    
  /* Specific display elements */
    /* Address picker */
    "address_list" : "address-list",
    "address_entry" : "address-entry",
    "address_entry_ownline" : "address-entry-ownline",
    
  /* Always declare last */
    "dummy" : ""
]);
