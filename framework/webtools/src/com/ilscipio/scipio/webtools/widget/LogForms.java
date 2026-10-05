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
package com.ilscipio.scipio.webtools.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LogForms {

    @Form(
        name = "LevelSwitch",
        location = "component://webtools/widget/LogForms.xml",
        target = "AdjustDebugLevels",
        defaultMapName = "activeDebugLevel",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fatal", title = "${uiLabelMap.WebtoolsFatalLogLevel}", tooltip = "${uiLabelMap.WebtoolsFatalLogLevelTooltip}", check = @CheckField),
            @FormField(name = "error", title = "${uiLabelMap.WebtoolsErrorLogLevel}", tooltip = "${uiLabelMap.WebtoolsErrorLogLevelTooltip}", check = @CheckField),
            @FormField(name = "warning", title = "${uiLabelMap.WebtoolsWarningLogLevel}", tooltip = "${uiLabelMap.WebtoolsWarningLogLevelTooltip}", check = @CheckField),
            @FormField(name = "important", title = "${uiLabelMap.WebtoolsImportantLogLevel}", tooltip = "${uiLabelMap.WebtoolsImportantLogLevelTooltip}", check = @CheckField),
            @FormField(name = "info", title = "${uiLabelMap.WebtoolsInfoLogLevel}", tooltip = "${uiLabelMap.WebtoolsInfoLogLevelTooltip}", check = @CheckField),
            @FormField(name = "timing", title = "${uiLabelMap.WebtoolsTimingLogLevel}", tooltip = "${uiLabelMap.WebtoolsTimingLogLevelTooltip}", check = @CheckField),
            @FormField(name = "verbose", title = "${uiLabelMap.WebtoolsVerboseLogLevel}", tooltip = "${uiLabelMap.WebtoolsVerboseLogLevelTooltip}", check = @CheckField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface LevelSwitch {}

}
