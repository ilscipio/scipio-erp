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
public class ServiceForms {

    @Form(
        name = "scheduleJob",
        location = "component://webtools/widget/ServiceForms.xml",
        target = "setServiceParameters",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "JOB_NAME", title = "${uiLabelMap.WebtoolsJob}", text = @TextField),
            @FormField(name = "SERVICE_NAME", title = "${uiLabelMap.WebtoolsService}", text = @TextField),
            @FormField(name = "POOL_NAME", title = "${uiLabelMap.WebtoolsPool}", text = @TextField),
            @FormField(name = "SERVICE_TIME", title = "${uiLabelMap.CommonStartDateTime}", tooltip = "+${uiLabelMap.WebtoolsLeaveEmptyToRunJobImmediately}", dateTime = @DateTimeField),
            @FormField(name = "SERVICE_END_TIME", title = "${uiLabelMap.CommonEndDateTime}", dateTime = @DateTimeField),
            @FormField(name = "SERVICE_FREQUENCY", title = "${uiLabelMap.WebtoolsFrequency}", dropDown = @DropDownField(options = {@Option(description = "${uiLabelMap.CommonNone}"), @Option(key = "4", description = "${uiLabelMap.CommonDaily}"), @Option(key = "5", description = "${uiLabelMap.CommonWeekly}"), @Option(key = "6", description = "${uiLabelMap.CommonMonthly}"), @Option(key = "7", description = "${uiLabelMap.CommonYearly}"), @Option(key = "3", description = "${uiLabelMap.CommonHourly}"), @Option(key = "2", description = "${uiLabelMap.CommonMinutely}"), @Option(key = "1", description = "${uiLabelMap.CommonSecondly}")})),
            @FormField(name = "SERVICE_INTERVAL", title = "${uiLabelMap.WebtoolsInterval}", tooltip = "${uiLabelMap.WebtoolsForUseWithFrequency}", text = @TextField),
            @FormField(name = "SERVICE_COUNT", title = "${uiLabelMap.WebtoolsCount}", tooltip = "${uiLabelMap.WebtoolsNumberOfTimeTheJobWillRun}", text = @TextField(defaultValue = "1")),
            @FormField(name = "SERVICE_MAXRETRY", title = "${uiLabelMap.WebtoolsMaxRetry}", tooltip = "${uiLabelMap.WebtoolsNumberOfJobRetry}", text = @TextField(defaultValue = "0")),
            @FormField(name = "SERVICE_EVENTID", title = "${uiLabelMap['StatusType.description.EVENT_STATUS']}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} (${enumId})", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "SCH_EVENT_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "SERVICE_PRIORITY", title = "${uiLabelMap.CommonPriority}", text = @TextField(placeholder = "50")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface scheduleJob {}

    @Form(
        name = "runService",
        location = "component://webtools/widget/ServiceForms.xml",
        target = "setSyncServiceParameters",
        focusFieldName = "SERVICE_NAME",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "SERVICE_NAME", title = "${uiLabelMap.WebtoolsService}", text = @TextField),
            @FormField(name = "POOL_NAME", title = "${uiLabelMap.WebtoolsPool}", text = @TextField),
            @FormField(name = "_RUN_SYNC_", title = "${uiLabelMap.WebtoolsMode}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "Sync"), @Option(key = "ASYNC", description = "Async (${uiLabelMap.CommonOneTimeExecNotPersistedResultsInLog})")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", submit = @SubmitField)
        }
    )
    public interface runService {}

    @Form(
        name = "FindJobs",
        location = "component://webtools/widget/ServiceForms.xml",
        target = "FindJob",
        defaultEntityName = "JobSandbox",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "jobName", title = "${uiLabelMap.WebtoolsJob}", textFind = @TextFindField),
            @FormField(name = "jobId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "serviceName", title = "${uiLabelMap.WebtoolsServiceName}", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "SERVICE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "eventId", title = "${uiLabelMap['StatusType.description.EVENT_STATUS']}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} (${enumId})", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "SCH_EVENT_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "poolId", title = "${uiLabelMap.WebtoolsPool}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindJobs {}

    @Form(
        name = "ListJobs",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "JobSandbox",
        paginateTarget = "FindJob",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "jobName", title = "${uiLabelMap.WebtoolsJob}", sortField = true, display = @DisplayField),
            @FormField(name = "jobId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "JobDetails", description = "${jobId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobId", fromField = "jobId")})),
            @FormField(name = "poolId", title = "${uiLabelMap.WebtoolsPool}", sortField = true, display = @DisplayField),
            @FormField(name = "runTime", title = "${uiLabelMap.WebtoolsRunTime}", sortField = true, display = @DisplayField),
            @FormField(name = "startDateTime", title = "${uiLabelMap.CommonStartDateTime}", sortField = true, display = @DisplayField),
            @FormField(name = "serviceName", title = "${uiLabelMap.WebtoolsService}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", sortField = true, hyperlink = @HyperlinkField(target = "ServiceList", description = "${serviceName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "sel_service_name", fromField = "serviceName")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "finishDateTime", title = "${uiLabelMap.CommonEndDateTime}", sortField = true, display = @DisplayField),
            @FormField(name = "eventId", title = "${uiLabelMap['StatusType.description.EVENT_STATUS']}", sortField = true, display = @DisplayField),
            @FormField(name = "priority", title = "${uiLabelMap.CommonPriority}", sortField = true, display = @DisplayField),
            @FormField(name = "cancelAction", title = " ", useWhen = "startDateTime==null&&finishDateTime==null&&cancelDateTime==null", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "cancelJob", description = "${uiLabelMap.WebtoolsCancelJob}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "jobCtx"), @FieldMap(fieldName = "entityName", value = "JobSandbox"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListJobs {}

    @Form(
        name = "JobDetails",
        location = "component://webtools/widget/ServiceForms.xml",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobSandbox", mapName = "job", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "jobResult", textarea = @TextareaField(readonly = true))
        }
    )
    public interface JobDetails {}

    @Form(
        name = "JobRuntimeDataInfo",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "runtimeInfoList",
        paginate = "false",
        fields = {
            @FormField(name = "key", display = @DisplayField),
            @FormField(name = "value", display = @DisplayField)
        }
    )
    public interface JobRuntimeDataInfo {}

    @Form(
        name = "PoolState",
        location = "component://webtools/widget/ServiceForms.xml",
        defaultMapName = "poolState",
        fields = {
            @FormField(name = "keepAliveTimeInSeconds", display = @DisplayField),
            @FormField(name = "numberOfCoreInvokerThreads", display = @DisplayField),
            @FormField(name = "currentNumberOfInvokerThreads", display = @DisplayField),
            @FormField(name = "numberOfActiveInvokerThreads", display = @DisplayField),
            @FormField(name = "maxNumberOfInvokerThreads", display = @DisplayField),
            @FormField(name = "greatestNumberOfInvokerThreads", display = @DisplayField),
            @FormField(name = "numberOfCompletedTasks", display = @DisplayField)
        }
    )
    public interface PoolState {}

    @Form(
        name = "ListJavaThread",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "threads",
        paginateTarget = "threadList",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "id", title = "${uiLabelMap.WebtoolsThread}", display = @DisplayField(description = "${threadId} ${threadName}")),
            @FormField(name = "name", title = "${uiLabelMap.WebtoolsJob}", display = @DisplayField(defaultValue = "${uiLabelMap.CommonNone}")),
            @FormField(name = "serviceName", title = "${uiLabelMap.WebtoolsService}", display = @DisplayField(defaultValue = "${uiLabelMap.CommonNone}")),
            @FormField(name = "time", title = "${uiLabelMap.CommonStartDateTime}", display = @DisplayField),
            @FormField(name = "runTime", title = "${uiLabelMap.CommonTime} (ms)", display = @DisplayField)
        }
    )
    public interface ListJavaThread {}

    @Form(
        name = "ListServices",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "services",
        paginateTarget = "ServiceLog",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "serviceName", title = "${uiLabelMap.WebtoolsServiceName}", sortField = true, display = @DisplayField),
            @FormField(name = "localName", title = "${uiLabelMap.WebtoolsDispatcherName}", sortField = true, display = @DisplayField),
            @FormField(name = "modeStr", title = "${uiLabelMap.WebtoolsMode}", sortField = true, display = @DisplayField(defaultValue = "${uiLabelMap.CommonNone}")),
            @FormField(name = "startTime", title = "${uiLabelMap.CommonStartDateTime}", sortField = true, display = @DisplayField),
            @FormField(name = "endTime", title = "${uiLabelMap.CommonEndDateTime}", sortField = true, display = @DisplayField(defaultValue = "${uiLabelMap.WebtoolsStatusRunning}"))
        }
    )
    public interface ListServices {}

    @Form(
        name = "FindJobManagerLock",
        location = "component://webtools/widget/ServiceForms.xml",
        target = "FindJobManagerLock",
        defaultEntityName = "JobManagerLock",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "instanceId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${enumCode} - ${description}", keyFieldName = "enumCode", constraints = {@EntityConstraint(name = "enumTypeId", value = "JM_OFBIZ_INSTANCE")}))),
            @FormField(name = "fromDate", dateFind = @DateFindField),
            @FormField(name = "thruDate", dateFind = @DateFindField),
            @FormField(name = "reasonEnumId", title = "${uiLabelMap.CommonReason}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "[${enumCode}] - ${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "JS_LOCK_REASON")}))),
            @FormField(name = "searchButton", submit = @SubmitField)
        }
    )
    public interface FindJobManagerLock {}

    @Form(
        name = "ListJobManagerLock",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "JobManagerLock",
        paginateTarget = "FindJobManagerLock",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        defaultTableStyle = "basic-table hover-bar",
        separateColumns = true,
        fields = {
            @FormField(name = "instanceId", sortField = true, display = @DisplayField),
            @FormField(name = "fromDate", sortField = true, display = @DisplayField),
            @FormField(name = "thruDate", sortField = true, display = @DisplayField),
            @FormField(name = "createdDate", sortField = true, display = @DisplayField),
            @FormField(name = "createdByUserLogin", sortField = true, display = @DisplayField),
            @FormField(name = "reasonEnumId", title = "${uiLabelMap.CommonReason}", sortField = true, displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "[${enumCode}] ${description}")),
            @FormField(name = "comments", display = @DisplayField),
            @FormField(name = "cancelButton", title = " ", useWhen = "editable && cancelable", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "updateJobManagerLock", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "instanceId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", value = "${nowTimestamp}")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "JobManagerLock"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField")})}),
        rowActions = @RowActions(set = {@SetAction(field = "editable", value = "${groovy: security.hasPermission('SERVICE_JM_LOCK', userLogin) }", type = "Boolean"), @SetAction(field = "cancelable", value = "${groovy: thruDate == null || thruDate.compareTo(nowTimestamp) > 0}", type = "Boolean")})
    )
    public interface ListJobManagerLock {}

    @Form(
        name = "AddJobManagerLock",
        location = "component://webtools/widget/ServiceForms.xml",
        target = "createJobManagerLock",
        defaultEntityName = "JobManagerLock",
        fields = {
            @FormField(name = "instanceId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${enumCode} - ${description}", keyFieldName = "enumCode", constraints = {@EntityConstraint(name = "enumTypeId", value = "JM_OFBIZ_INSTANCE")}))),
            @FormField(name = "fromDate", dateTime = @DateTimeField),
            @FormField(name = "thruDate", dateTime = @DateTimeField),
            @FormField(name = "reasonEnumId", title = "${uiLabelMap.CommonReason}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "[${enumCode}] - ${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "JS_LOCK_REASON")}))),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "addButton", submit = @SubmitField)
        },
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "submit", areaId = "window", areaTarget = "FindJobManagerLock")
        }
    )
    public interface AddJobManagerLock {}

    @Form(
        name = "JobManagerLockEnable",
        location = "component://webtools/widget/ServiceForms.xml",
        type = FormType.LIST,
        listName = "jobManagerLocks",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        defaultTableStyle = "basic-table hover-bar",
        fields = {
            @FormField(name = "instanceId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", redWhen = "never", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", redWhen = "never", display = @DisplayField(type = "date-time")),
            @FormField(name = "createdByUserLogin", title = "${uiLabelMap.CommonBy}", display = @DisplayField),
            @FormField(name = "reasonEnumId", title = "${uiLabelMap.CommonReason}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "[${enumCode}] ${description}")),
            @FormField(name = "cancelButton", title = " ", useWhen = "editable && cancelable", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "updateJobManagerLock", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "instanceId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", value = "${nowTimestamp}")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "editable", value = "${groovy: security.hasPermission('SERVICE_JM_LOCK', userLogin) }", type = "Boolean"), @SetAction(field = "cancelable", value = "${groovy: thruDate == null || thruDate.compareTo(nowTimestamp) > 0}", type = "Boolean")})
    )
    public interface JobManagerLockEnable {}

}
