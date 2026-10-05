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
 * SCIPIO: Prepares request and widget context for new render.
 * <p>
 * Invoked by the screen renderer upon start of view/screen rendering.
 * <p>
 * This is essential to:
 * - clean up request vars across servlet forwards
 */

import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;
import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;

final module = "PrepareNewScreenRender.groovy"

Debug.logInfo("Scipio: Preparing new templating API render; clearing request-scope templating vars", module);

// note: env param will usually be null here, but just in case...
env = FreeMarkerWorker.getCurrentEnvironment();
ContextFtlUtil.resetRequestVars(context.request, context, env);
