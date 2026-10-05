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
public class EntityForms {

    @Form(
        name = "EntityEoModelBundle",
        location = "component://webtools/widget/EntityForms.xml",
        target = "exportEntityEoModelBundle",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "exportEntityEoModelBundle")
        },
        fields = {
            @FormField(name = "eomodeldFullPath", text = @TextField(size = 100)),
            @FormField(name = "entityGroupId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EntityGroup", description = "${entityGroupName}", orderBy = {@EntityOrderBy(fieldName = "entityGroupName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface EntityEoModelBundle {}

    @Form(
        name = "ListPerformanceResults",
        location = "component://webtools/widget/EntityForms.xml",
        type = FormType.LIST,
        listName = "performanceList",
        paginateTarget = "EntityPerformanceTest",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "operation", title = "${uiLabelMap.WebtoolsPerformanceOperation}", display = @DisplayField),
            @FormField(name = "entity", title = "${uiLabelMap.WebtoolsEntity}", display = @DisplayField),
            @FormField(name = "calls", title = "${uiLabelMap.WebtoolsPerformanceCalls}", display = @DisplayField),
            @FormField(name = "seconds", title = "${uiLabelMap.WebtoolsPerformanceSeconds}", display = @DisplayField),
            @FormField(name = "secsPerCall", title = "${uiLabelMap.WebtoolsPerformanceSecondsCall}", display = @DisplayField),
            @FormField(name = "callsPerSecond", title = "${uiLabelMap.WebtoolsPerformanceCallsSecond}", display = @DisplayField)
        }
    )
    public interface ListPerformanceResults {}

}
