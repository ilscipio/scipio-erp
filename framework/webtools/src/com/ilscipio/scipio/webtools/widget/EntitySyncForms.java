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
public class EntitySyncForms {

    @Form(
        name = "EntitySyncStatus",
        location = "component://webtools/widget/EntitySyncForms.xml",
        type = FormType.LIST,
        listName = "entitySyncList",
        paginateTarget = "EntitySyncStatus",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EntitySync", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "runStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "resetStatus", title = " ", useWhen = "\"ESR_RUNNING\".equals(runStatusId)", widgetStyle = "${styles.link_run_sys} ${styles.action_clear}", hyperlink = @HyperlinkField(target = "resetEntitySyncStatusToNotStarted", description = "${uiLabelMap.WebtoolsSyncResetRunStatus}", alsoHidden = false, parameters = {@ParameterDef(paramName = "entitySyncId")})),
            @FormField(name = "runOfflineSync", title = " ", useWhen = "\"ESR_NOT_STARTED\".equals(runStatusId) || \"ESR_COMPLETE\".equals(runStatusId)", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", hyperlink = @HyperlinkField(target = "runOfflineEntitySync", description = "${uiLabelMap.WebtoolsSyncRunOffline}", alsoHidden = false, parameters = {@ParameterDef(paramName = "entitySyncId")})),
            @FormField(name = "acceptOffline", title = " ", useWhen = "\"ESR_PENDING\".equals(runStatusId)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updateOfflineEntitySync", description = "${uiLabelMap.WebtoolsSyncAcceptOffline}", alsoHidden = false, parameters = {@ParameterDef(paramName = "entitySyncId"), @ParameterDef(paramName = "updateType", value = "ACCEPT")})),
            @FormField(name = "rejectOffline", title = " ", useWhen = "\"ESR_PENDING\".equals(runStatusId)", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "updateOfflineEntitySync", description = "${uiLabelMap.WebtoolsSyncRejectOffline}", alsoHidden = false, parameters = {@ParameterDef(paramName = "entitySyncId"), @ParameterDef(paramName = "updateType", value = "REJECT")}))
        }
    )
    public interface EntitySyncStatus {}

    @Form(
        name = "EntitySyncLoadOffline",
        location = "component://webtools/widget/EntitySyncForms.xml",
        target = "loadOfflineEntitySyncData",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "loadOfflineEntitySyncData")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface EntitySyncLoadOffline {}

}
