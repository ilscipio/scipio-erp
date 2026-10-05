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
 * Sets the ftl library variables (styles hash) into render context, for use even outside of ftl (SCIPIO).
 */
import org.ofbiz.base.util.*;
import org.ofbiz.widget.renderer.VisualThemeWorker;

final module = "SetContextScipioTmplGlobalVars.groovy"

scpLibVarsRaw = VisualThemeWorker.getFtlLibVariables(context);
if (scpLibVarsRaw != null) {
    if (Boolean.TRUE.equals(context.setContextScipioTmplGlobalVarsAsGlobal)) {
        globalContext.putAll(scpLibVarsRaw);
    } else {
        context.putAll(scpLibVarsRaw);
    }
} else {
    Debug.logWarning("No valid Scipio theme/template variables found in theme or system - is theme or system set up correctly?", module);
}



