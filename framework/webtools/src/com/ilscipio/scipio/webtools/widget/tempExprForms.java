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
public class tempExprForms {

    @Form(
        name = "FindTemporalExpression",
        location = "component://webtools/widget/tempExprForms.xml",
        target = "findTemporalExpression",
        fields = {
            @FormField(name = "tempExprId", title = "${uiLabelMap.TemporalExpressionId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "expressionTypeList", value = "${groovy:org.ofbiz.service.calendar.ExpressionUiHelper.getExpressionTypeList(uiLabelMap);}", type = "List")})
    )
    public interface FindTemporalExpression {}

    @Form(
        name = "BasicExpressionList",
        location = "component://webtools/widget/tempExprForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "tempExprId", title = "${uiLabelMap.TemporalExpressionId}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "editTemporalExpression", urlMode = UrlMode.PLAIN, description = "${tempExprId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "tempExprId")})),
            @FormField(name = "tempExprTypeId", title = "${uiLabelMap.TemporalExpressionType}", sortField = true, display = @DisplayField),
            @FormField(name = "description", sortField = true, display = @DisplayField),
            @FormField(name = "date1", sortField = true, display = @DisplayField),
            @FormField(name = "date2", sortField = true, display = @DisplayField),
            @FormField(name = "integer1", sortField = true, display = @DisplayField),
            @FormField(name = "integer2", sortField = true, display = @DisplayField),
            @FormField(name = "string1", sortField = true, display = @DisplayField),
            @FormField(name = "string2", sortField = true, display = @DisplayField)
        }
    )
    public interface BasicExpressionList {}

    @Form(
        name = "ListTemporalExpressions",
        location = "component://webtools/widget/tempExprForms.xml",
        listName = "listIt",
        extendsForm = "BasicExpressionList",
        paginate = "true",
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "tempExprId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "TemporalExpression"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTemporalExpressions {}

    @Form(
        name = "ListChildExpressions",
        location = "component://webtools/widget/tempExprForms.xml",
        listName = "childExpressionList",
        extendsForm = "BasicExpressionList",
        paginateTarget = "editTemporalExpression?tempExprId=${tempExprId}",
        fields = {
            @FormField(name = "exprAssocType", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTemporalExpressionAssoc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "tempExprId", fromField = "parameters.tempExprId"), @ParameterDef(paramName = "fromTempExprId"), @ParameterDef(paramName = "toTempExprId", fromField = "tempExprId")}))
        }
    )
    public interface ListChildExpressions {}

}
