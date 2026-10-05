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
 * SCIPIO: This script merely invokes another script name dynamically. 
 * In stock can't do this from screens because script loc not context-aware.
 * TODO: support more scripts/args?
 */

import org.ofbiz.base.util.*
import org.ofbiz.base.util.ScriptUtil

if (context.scriptLocation) {
    ScriptUtil.executeScript(context.scriptLocation, context.scriptFunction, context)
}
else {
    Debug.logError("Missing scriptLocation", "runScript.groovy")
}
