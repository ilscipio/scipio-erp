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
package com.ilscipio.scipio.humanres.widget;

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
public class FormsPayGradeForms {

    @Form(
        name = "FindPayGrades",
        location = "component://humanres/widget/forms/PayGradeForms.xml",
        target = "FindPayGrades",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPayGrade", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "payGradeId", lookup = @LookupField(targetFormName = "LookupPayGrade")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPayGrades {}

    @Form(
        name = "ListPayGrades",
        location = "component://humanres/widget/forms/PayGradeForms.xml",
        type = FormType.LIST,
        target = "updatePayGrade",
        listName = "listIt",
        paginateTarget = "FindPayGrade",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePayGrade", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "payGradeId", title = "${uiLabelMap.HumanResPayGradeID}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditPayGrade", urlMode = UrlMode.PLAIN, description = "${payGradeId}", parameters = {@ParameterDef(paramName = "payGradeId")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePayGrade", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "payGradeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "payGradeCtx"), @FieldMap(fieldName = "entityName", value = "PayGrade"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPayGrades {}

    @Form(
        name = "EditPayGrade",
        location = "component://humanres/widget/forms/PayGradeForms.xml",
        target = "updatePayGrade",
        defaultMapName = "payGrade",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePayGrade")
        },
        fields = {
            @FormField(name = "payGradeId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "payGrade!=null", display = @DisplayField),
            @FormField(name = "payGradeId", useWhen = "payGrade==null", text = @TextField),
            @FormField(name = "payGradeName", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "payGrade!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "payGradeId==null", target = "createPayGrade")
        }
    )
    public interface EditPayGrade {}

    @Form(
        name = "ListSalarySteps",
        location = "component://humanres/widget/forms/PayGradeForms.xml",
        type = FormType.MULTI,
        target = "updateSalaryStep?salaryStepSeqId=${salaryStepSeqId}&payGradeId=${payGradeId}",
        paginateTarget = "findSalarySteps",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSalaryStep")
        },
        fields = {
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResSalaryStepSeqId}", display = @DisplayField),
            @FormField(name = "payGradeId", hidden = @HiddenField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSalaryStep", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "salaryStepSeqId"), @ParameterDef(paramName = "payGradeId")}))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "SalaryStep")})
    )
    public interface ListSalarySteps {}

    @Form(
        name = "AddSalaryStep",
        location = "component://humanres/widget/forms/PayGradeForms.xml",
        target = "createSalaryStep",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSalaryStep")
        },
        fields = {
            @FormField(name = "salaryStepSeqId", ignored = @IgnoredField),
            @FormField(name = "payGradeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSalaryStep {}

}
