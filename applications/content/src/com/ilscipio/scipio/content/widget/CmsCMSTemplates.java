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
package com.ilscipio.scipio.content.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CmsCMSTemplates {

    @Screen(name = "ContentOnly", location = "component://content/widget/cms/CMSTemplates.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "MAIN", editRequest = "EditAddSubContent?MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "enableEdit")}))
    public interface ContentOnly {}

    @Screen(name = "FloatLeft", location = "component://content/widget/cms/CMSTemplates.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "MAIN", editRequest = "EditAddSubContent?MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "enableEdit", position = 1)}, containers = {@Container(style = "floatleft", widgets = {@Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "AUX", editRequest = "EditAddSubContent?MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "enableEdit")}, position = 0)}))
    public interface FloatLeft {}

    @Screen(name = "TopCenter", location = "component://content/widget/cms/CMSTemplates.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "MAIN", editRequest = "EditAddSubContent?MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "enableEdit", position = 1)}, containers = {@Container(style = "topcentered", widgets = {@Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "AUX", editRequest = "EditAddSubContent?MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "enableEdit")}, position = 0)}))
    public interface TopCenter {}

}
