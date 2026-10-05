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
public class ArtifactInfoForms {

    @Form(
        name = "ComponentList",
        location = "component://webtools/widget/ArtifactInfoForms.xml",
        type = FormType.LIST,
        title = "Component List",
        listName = "componentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "compName", widgetStyle = "${styles.link_nav_info_name} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewComponent?compName=${compName}", description = "${compName}")),
            @FormField(name = "rootLocation", display = @DisplayField(description = "${relRootLoc}")),
            @FormField(name = "enabled", display = @DisplayField),
            @FormField(name = "webAppName", display = @DisplayField),
            @FormField(name = "contextRoot", useWhen = "contextRootLinkUri!=null", hyperlink = @HyperlinkField(target = "${contextRootLinkUri}", urlMode = UrlMode.INTER_APP, description = "${contextRoot}", targetWindow = "_blank")),
            @FormField(name = "contextRoot", useWhen = "contextRootLinkUri==null", display = @DisplayField),
            @FormField(name = "location", display = @DisplayField(description = "${relWebLoc}"))
        }
    )
    public interface ComponentList {}

    @Form(
        name = "TestSuiteInfo",
        location = "component://webtools/widget/ArtifactInfoForms.xml",
        type = FormType.LIST,
        title = "Component List",
        listName = "suits",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "compName", hidden = @HiddenField(value = "${parameters.compName}")),
            @FormField(name = "suiteName", display = @DisplayField),
            @FormField(name = "runSuite", useWhen = "suiteName!=void", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", hyperlink = @HyperlinkField(target = "RunTest?compName=${parameters.compName}&suiteName=${suiteNameSave}", description = "run suite")),
            @FormField(name = "caseName", display = @DisplayField),
            @FormField(name = "runCase", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", hyperlink = @HyperlinkField(target = "RunTest?compName=${parameters.compName}&suiteName=${suiteNameSave}&caseName=${caseName}", description = "run case"))
        }
    )
    public interface TestSuiteInfo {}

}
