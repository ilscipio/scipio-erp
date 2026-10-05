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
public class MiscForms {

    @Form(
        name = "ProgramExport",
        location = "component://webtools/widget/MiscForms.xml",
        target = "ProgramExport",
        defaultMapName = "parameters",
        fields = {
            @FormField(name = "groovyProgram", requiredField = true, textarea = @TextareaField(cols = 120, rows = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ProgramExport {}

    @Form(
        name = "LayoutDemoForm",
        location = "component://webtools/widget/MiscForms.xml",
        target = "${demoTargetUrl}",
        defaultMapName = "demoMap",
        fields = {
            @FormField(name = "name", title = "${uiLabelMap.CommonName}", requiredField = true, text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "dropDown", title = "${uiLabelMap.CommonEnabled}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "checkBox", title = "${uiLabelMap.CommonEnabled}", check = @CheckField),
            @FormField(name = "radioButton", title = "${uiLabelMap.CommonEnabled}", radio = @RadioField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "myFormPassedVar1", title = "myFormPassedVar1", display = @DisplayField(description = "${groovy: context.myFormPassedVar1 ?: 'missing'}")),
            @FormField(name = "myFormPassedGlobalVar1", title = "myFormPassedGlobalVar1", display = @DisplayField(description = "${groovy: context.myFormPassedGlobalVar1 ?: 'missing'}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${demoTargetUrl}", description = "${uiLabelMap.CommonCancel}"))
        }
    )
    public interface LayoutDemoForm {}

    @Form(
        name = "LayoutDemoList",
        location = "component://webtools/widget/MiscForms.xml",
        type = FormType.LIST,
        listName = "demoList",
        paginateTarget = "${demoTargetUrl}",
        headerRowStyle = "${headerStyle}",
        oddRowStyle = "${altRowStyle}",
        defaultTableStyle = "${tableStyle}",
        separateColumns = true,
        fields = {
            @FormField(name = "name", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "dropDown", title = "${uiLabelMap.CommonEnabled}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "checkBox", title = "${uiLabelMap.CommonEnabled}", check = @CheckField),
            @FormField(name = "radioButton", title = "${uiLabelMap.CommonEnabled}", radio = @RadioField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update} button-col", submit = @SubmitField)
        }
    )
    public interface LayoutDemoList {}

    @Form(
        name = "LayoutDemoActionsIncludeTest1",
        location = "component://webtools/widget/MiscForms.xml",
        actions = @FormActions(set = {@SetAction(field = "commonActionField2", value = "This value 2 was set in LayoutDemoActionsIncludeTest1 screen actions included using the new include-actions screen widget directive. [SUCCESS]"), @SetAction(field = "commonActionField3", value = "This value 3 was set in LayoutDemoActionsIncludeTest1 screen actions included using the new include-actions screen widget directive, but should be overridden child. [ERROR]")})
    )
    public interface LayoutDemoActionsIncludeTest1 {}

    @Form(
        name = "LayoutDemoActionsIncludeTest2",
        location = "component://webtools/widget/MiscForms.xml",
        extendsForm = "LayoutDemoActionsIncludeTest1",
        actions = @FormActions(set = {@SetAction(field = "commonActionField3", value = "This value 3 was set in LayoutDemoActionsIncludeTest2 screen actions included using the new include-actions screen widget directive. [SUCCESS]")})
    )
    public interface LayoutDemoActionsIncludeTest2 {}

    @Form(
        name = "TargetedRenderingTestForm1",
        location = "component://webtools/widget/MiscForms.xml",
        target = "TargetedRenderingTest",
        fields = {
            @FormField(name = "testinput1", title = "Input 1", text = @TextField),
            @FormField(name = "testinput1", title = "Input 2", text = @TextField)
        }
    )
    public interface TargetedRenderingTestForm1 {}

    @Form(
        name = "TooltipTestForm1",
        location = "component://webtools/widget/MiscForms.xml",
        fields = {
            @FormField(name = "input1", tooltip = "This is a tooltip!", text = @TextField),
            @FormField(name = "display1", tooltip = "This is a tooltip!", display = @DisplayField),
            @FormField(name = "check1", tooltip = "This is a tooltip!", check = @CheckField),
            @FormField(name = "radio1", tooltip = "This is a tooltip!", radio = @RadioField),
            @FormField(name = "dateTime1", tooltip = "This is a tooltip!", dateTime = @DateTimeField),
            @FormField(name = "displayEntity1", mapName = "party", entryName = "partyId", tooltip = "This is a tooltip!", displayEntity = @DisplayEntityField(entityName = "Party", keyFieldName = "partyId")),
            @FormField(name = "file1", tooltip = "This is a tooltip!", file = @FileField),
            @FormField(name = "lookup1", tooltip = "This is a tooltip!", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "password1", tooltip = "This is a tooltip!", password = @PasswordField),
            @FormField(name = "rangeFind1", tooltip = "This is a tooltip!", rangeFind = @RangeFindField),
            @FormField(name = "dateFind1", tooltip = "This is a tooltip!", dateFind = @DateFindField),
            @FormField(name = "textFind1", tooltip = "This is a tooltip!", textFind = @TextFindField),
            @FormField(name = "dropDown1", tooltip = "This is a tooltip!", dropDown = @DropDownField),
            @FormField(name = "reset1", tooltip = "This is a tooltip!", reset = @ResetField),
            @FormField(name = "image1", tooltip = "This is a tooltip!", image = @ImageField(value = "/images/scipio/scipio-logo-small.png")),
            @FormField(name = "hyperlink1", tooltip = "This is a tooltip!", hyperlink = @HyperlinkField(target = "LayoutDemo", description = "This is a link")),
            @FormField(name = "textarea1", tooltip = "This is a tooltip!", textarea = @TextareaField),
            @FormField(name = "submit1", tooltip = "This is a tooltip!", submit = @SubmitField),
            @FormField(name = "submit2", tooltip = "This is a tooltip!", submit = @SubmitField(buttonType = "text-link"))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "Party", valueField = "party")})
    )
    public interface TooltipTestForm1 {}

    @Form(
        name = "ActionsTestForm1",
        location = "component://webtools/widget/MiscForms.xml",
        fields = {
            @FormField(name = "myTestField1", useWhen = "myTestField1==null", display = @DisplayField(description = "myTestField1 value was null")),
            @FormField(name = "myTestField1", useWhen = "myTestField1!=null", display = @DisplayField(description = "myTestField1 value was not null")),
            @FormField(name = "myTestField2", useWhen = "myTestField2==null", display = @DisplayField(description = "myTestField2 value was null")),
            @FormField(name = "myTestField2", useWhen = "myTestField2!=null", display = @DisplayField(description = "myTestField2 value was not null")),
            @FormField(name = "myTestField3", useWhen = "${myTestField3434 == 'hello'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isEmpty(myTestField1)}", display = @DisplayField(description = "myTestField3434 was hello and myTestField1 was empty"))
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/ActionsTestForm1_script1.groovy")})
    )
    public interface ActionsTestForm1 {}

}
