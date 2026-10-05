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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingCalendarForms {

    @Form(
        name = "ListTechDataCalendars",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        type = FormType.LIST,
        listName = "techDataCalendars",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "calendarId", title = "${uiLabelMap.ManufacturingCalendarId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeekId}", display = @DisplayField),
            @FormField(name = "updateAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditCalendar", description = "${uiLabelMap.CommonUpdate}", parameters = {@ParameterDef(paramName = "calendarId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveCalendar", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "calendarId")}))
        }
    )
    public interface ListTechDataCalendars {}

    @Form(
        name = "ListCalendarWeek",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        type = FormType.LIST,
        target = "updateCalendarWeek",
        listName = "calendarWeeks",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeekId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "updateAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditCalendarWeek", description = "${uiLabelMap.CommonUpdate}", parameters = {@ParameterDef(paramName = "calendarWeekId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveCalendarWeek", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "calendarWeekId")}))
        }
    )
    public interface ListCalendarWeek {}

    @Form(
        name = "ListCalendarExceptionDay",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        type = FormType.LIST,
        target = "UpdateCalendarExceptionDay",
        listName = "calendarExceptionDays",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "calendarId", hidden = @HiddenField),
            @FormField(name = "exceptionDateStartTime", title = "${uiLabelMap.ManufacturingExceptionDateStartTime}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "exceptionCapacity", title = "${uiLabelMap.ManufacturingCalendarCapacity}", display = @DisplayField),
            @FormField(name = "usedCapacity", title = "${uiLabelMap.ManufacturingUsedCapacity}", display = @DisplayField),
            @FormField(name = "updateAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditCalendarExceptionDay", description = "${uiLabelMap.CommonSelect}", parameters = {@ParameterDef(paramName = "calendarId"), @ParameterDef(paramName = "exceptionDateStartTime")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveCalendarExceptionDay", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "calendarId"), @ParameterDef(paramName = "exceptionDateStartTime")}))
        }
    )
    public interface ListCalendarExceptionDay {}

    @Form(
        name = "UpdateCalendarExceptionDay",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        target = "UpdateCalendarExceptionDay",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCalendarExceptionDay", mapName = "calendarExceptionDay")
        },
        fields = {
            @FormField(name = "calendarId", hidden = @HiddenField),
            @FormField(name = "exceptionDateStartTime", title = "${uiLabelMap.ManufacturingExceptionDateStartTime}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", position = 2),
            @FormField(name = "exceptionCapacity", title = "${uiLabelMap.ManufacturingCalendarCapacity}", position = 3),
            @FormField(name = "usedCapacity", title = "${uiLabelMap.ManufacturingUsedCapacity}", position = 4),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", position = 5, submit = @SubmitField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "exceptionDateStartTime"), @SortField(name = "description"), @SortField(name = "exceptionCapacity"), @SortField(name = "usedCapacity")})
    )
    public interface UpdateCalendarExceptionDay {}

    @Form(
        name = "AddCalendarExceptionDay",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        target = "CreateCalendarExceptionDay",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCalendarExceptionDay", mapName = "calendarExceptionDay")
        },
        fields = {
            @FormField(name = "calendarId", mapName = "techDataCalendar", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "exceptionDateStartTime", title = "${uiLabelMap.ManufacturingExceptionDateStartTime}"),
            @FormField(name = "exceptionCapacity", title = "${uiLabelMap.ManufacturingCalendarCapacity}"),
            @FormField(name = "usedCapacity", title = "${uiLabelMap.ManufacturingUsedCapacity}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCalendarExceptionDay {}

    @Form(
        name = "ListCalendarExceptionWeek",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        type = FormType.LIST,
        target = "UpdateCalendarExceptionWeek",
        listName = "calendarExceptionWeeksDatas",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCalendarExceptionWeek", mapName = "calendarExceptionWeek")
        },
        fields = {
            @FormField(name = "calendarId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "exceptionDateStart", title = "${uiLabelMap.ManufacturingExceptionDateStart}", display = @DisplayField),
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeek}", display = @DisplayField(description = "${calendarWeek.description} ")),
            @FormField(name = "updateAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditCalendarExceptionWeek", description = "${uiLabelMap.CommonSelect}", parameters = {@ParameterDef(paramName = "calendarId", fromField = "calendarExceptionWeek.calendarId"), @ParameterDef(paramName = "exceptionDateStart", fromField = "calendarExceptionWeek.exceptionDateStart")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveCalendarExceptionWeek", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "calendarId", fromField = "calendarExceptionWeek.calendarId"), @ParameterDef(paramName = "exceptionDateStart", fromField = "calendarExceptionWeek.exceptionDateStart")}))
        }
    )
    public interface ListCalendarExceptionWeek {}

    @Form(
        name = "UpdateCalendarExceptionWeek",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        target = "UpdateCalendarExceptionWeek",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCalendarExceptionWeek", mapName = "calendarExceptionWeek")
        },
        fields = {
            @FormField(name = "calendarId", hidden = @HiddenField),
            @FormField(name = "exceptionDateStart", title = "${uiLabelMap.ManufacturingExceptionDateStart}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", position = 2),
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeekId}", position = 3, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TechDataCalendarWeek", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", position = 4, submit = @SubmitField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "exceptionDateStart"), @SortField(name = "description"), @SortField(name = "calendarWeekId")})
    )
    public interface UpdateCalendarExceptionWeek {}

    @Form(
        name = "AddCalendarExceptionWeek",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        target = "CreateCalendarExceptionWeek",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCalendarExceptionWeek", mapName = "calendarExceptionWeek")
        },
        fields = {
            @FormField(name = "calendarId", mapName = "techDatacalendar", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "exceptionDateStart", title = "${uiLabelMap.ManufacturingExceptionDateStart}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeekId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TechDataCalendarWeek", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCalendarExceptionWeek {}

    @Form(
        name = "UpdateCalendarWeek",
        location = "component://manufacturing/widget/manufacturing/CalendarForms.xml",
        target = "updateCalendarWeek",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCalendarWeek", mapName = "calendarWeek")
        },
        fields = {
            @FormField(name = "calendarWeekId", title = "${uiLabelMap.ManufacturingCalendarWeekId}", useWhen = "calendarWeek!=null", display = @DisplayField),
            @FormField(name = "mondayStartTime", text = @TextField),
            @FormField(name = "mondayCapacity", position = 2, text = @TextField),
            @FormField(name = "tuesdayStartTime", text = @TextField),
            @FormField(name = "tuesdayCapacity", position = 2, text = @TextField),
            @FormField(name = "wednesdayStartTime", text = @TextField),
            @FormField(name = "wednesdayCapacity", position = 2, text = @TextField),
            @FormField(name = "thursdayStartTime", text = @TextField),
            @FormField(name = "thursdayCapacity", position = 2, text = @TextField),
            @FormField(name = "fridayStartTime", text = @TextField),
            @FormField(name = "fridayCapacity", position = 2, text = @TextField),
            @FormField(name = "saturdayStartTime", text = @TextField),
            @FormField(name = "saturdayCapacity", position = 2, text = @TextField),
            @FormField(name = "sundayStartTime", text = @TextField),
            @FormField(name = "sundayCapacity", position = 2, text = @TextField),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "calendarWeek==null", target = "createCalendarWeek")
        }
    )
    public interface UpdateCalendarWeek {}

}
