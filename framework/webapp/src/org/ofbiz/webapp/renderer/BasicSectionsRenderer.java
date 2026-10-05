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
package org.ofbiz.webapp.renderer;

import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;

import freemarker.template.TemplateException;

/**
 * SCIPIO: Base sections renderer interface, so that the legacy ofbiz widget renderer "sections"
 * object can be replaced with compatible alternatives.
 */
public interface BasicSectionsRenderer {

    /**
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker
     * <p>
     * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
     * */
    String render(String sectionName, boolean asString) throws GeneralException, IOException, TemplateException;

    /**
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker
     */
    String render(String sectionName) throws GeneralException, IOException, TemplateException;

    /**
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
     */
    String renderScoped(String sectionName, Boolean asString, Boolean shareScope) throws GeneralException, IOException, TemplateException;

    /**
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * generic object/ftl-friendly version.
     */
    String renderScopedGen(String sectionName, Object asString, Object shareScope) throws GeneralException, IOException, TemplateException;

    /**
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * and optionally sets some context vars after the pushing.
     */
    String renderScoped(String sectionName, Boolean asString, Boolean shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException, TemplateException;

    /**
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * and optionally sets some context vars after the pushing.
     */
    String renderScopedGen(String sectionName, Object asString, Object shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException, TemplateException;

}
