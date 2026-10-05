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
package org.ofbiz.widget.model.ftl;

import java.util.Map;

import org.ofbiz.widget.model.ContainsExpr;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.model.ModelWidgetVisitor;

/**
 * TODO: Special wrapper for FTL elements to pass off as widgets.
 * Currently useless, no support for FTL matching and support uncertain.
 */
@SuppressWarnings("serial")
public class ModelFtlWidget extends ModelWidget implements FtlWrapperWidget, ModelWidget.IdAttrWidget, ContainsExpr.FlexibleContainsExprAttrWidget {
    private final String dirName;
    private final String location;
    private final String id;
    private final ContainsExpr containsExpr;

    public ModelFtlWidget(String name, String dirName, String location, String id, String containsExpr) {
        super(name != null ? name : "");
        this.dirName = dirName;
        this.location = location;
        this.id = id;
        this.containsExpr = ContainsExpr.getInstanceOrDefault(containsExpr);
    }

    public ModelFtlWidget(String name, String dirName, String location, String id) {
        this(name, dirName, location, id, null);
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
    }

    @Override
    public String getContainerLocation() {
        return location;
    }

    @Override
    public String getWidgetType() {
        // WARN: we have to prefix this otherwise there's a risk we'll interfere with widget names
        return "ftl-" + dirName;
    }

    @Override
    public String getTagName() {
        return dirName;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public ContainsExpr getContainsExpr(Map<String, Object> context) {
        return containsExpr;
    }
}