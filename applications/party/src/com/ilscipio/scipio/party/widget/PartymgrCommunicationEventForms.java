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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrCommunicationEventForms {

    @Form(
        name = "EditCommEvent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "updateCommunicationEvent",
        defaultMapName = "communicationEvent",
        defaultPositionSpan = 1,
        fields = {
            @FormField(name = "action", hidden = @HiddenField(value = "${parameters.action}")),
            @FormField(name = "communicationEventId", useWhen = "communicationEvent!=null", display = @DisplayField),
            @FormField(name = "my", hidden = @HiddenField(value = "${my}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "contactListId", position = 2, lookup = @LookupField(targetFormName = "LookupContactList", size = 20)),
            @FormField(name = "communicationEventTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CommunicationEventType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "parentCommEventId", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "reasonEnumId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CE_COMM_REASON")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "contactMechTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}"))),
            @FormField(name = "contactMechIdFrom", title = "${uiLabelMap.PartyFromContactMech}", lookup = @LookupField(targetFormName = "LookupPreferredContactMech", descriptionFieldName = "partyIdFrom")),
            @FormField(name = "contactMechIdTo", title = "${uiLabelMap.PartyToContactMech}", position = 2, lookup = @LookupField(targetFormName = "LookupPreferredContactMech", descriptionFieldName = "partyIdTo")),
            @FormField(name = "roleTypeIdFrom", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contentMimeTypeId", encodeOutput = false, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateTime = @DateTimeField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "subject", text = @TextField(size = 60)),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", textarea = @TextareaField(rows = 10)),
            @FormField(name = "note", title = "${uiLabelMap.CommonNote}", textarea = @TextareaField(rows = 3)),
            @FormField(name = "messageId", useWhen = "communicationEvent!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "communicationEvent==null", target = "createCommunicationEvent")
        },
        sortOrder = @SortOrder()
    )
    public interface EditCommEvent {}

    @Form(
        name = "EditEmail",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEvent",
        id = "EditEmail",
        defaultMapName = "communicationEvent",
        fields = {
            @FormField(name = "form", hidden = @HiddenField),
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "communicationEventTypeId", hidden = @HiddenField(value = "EMAIL_COMMUNICATION")),
            @FormField(name = "contactMechIdTo", hidden = @HiddenField(value = "${contactMechIdTo}")),
            @FormField(name = "statusId", hidden = @HiddenField(value = "COM_IN_PROGRESS")),
            @FormField(name = "parentCommEventId", useWhen = "parentCommEventId != null", hidden = @HiddenField(value = "${parameters.parentCommEventId}")),
            @FormField(name = "parentCommEventId", useWhen = "origCommEventId != null", hidden = @HiddenField(value = "${parameters.origCommEventId}")),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "my", hidden = @HiddenField(value = "${my}")),
            @FormField(name = "fromEmailAddr", parameterName = "contactMechIdFrom", dropDown = @DropDownField(listOptions = @ListOptions(listName = "emailAddresses", keyName = "contactMechId", description = "${infoString}"))),
            @FormField(name = "partyIdTo", tooltip = "Email: ${contactMechTo.infoString}", useWhen = "contactMechTo!=null", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${partyIdTo}")),
            @FormField(name = "partyIdTo", useWhen = "contactMechTo==null", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${partyIdTo}")),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonSendDate}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "subject", text = @TextField(size = 74, defaultValue = "${parameters.subject}")),
            @FormField(name = "contentMimeTypeId", hidden = @HiddenField(value = "text/plain")),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", encodeOutput = false, textarea = @TextareaField(cols = 72, rows = 15, defaultValue = "${parameters.content}")),
            @FormField(name = "sendAction", title = " ", useWhen = "communicationEvent!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_send}", hyperlink = @HyperlinkField(target = "javascript:(document.EditEmail.form.value='list'),(document.EditEmail.submit())", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSend}", alsoHidden = false)),
            @FormField(name = "saveAction", title = " ", useWhen = "communicationEvent!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", position = 2, hyperlink = @HyperlinkField(target = "javascript:(document.EditEmail.form.value='list'),(document.EditEmail.statusId.value='COM_PENDING'),(document.EditEmail.submit())", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSave}", alsoHidden = false)),
            @FormField(name = "createAction", useWhen = "communicationEvent==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "communicationEvent!=null", target = "sendCommunicationEvent")
        },
        actions = @FormActions(set = {@SetAction(field = "nowDate", value = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"yyyy-MM-dd HH:mm:ss.S\")}", type = "String")}, entityOne = {@EntityOneAction(entityName = "ContactMech", valueField = "contactMechTo")})
    )
    public interface EditEmail {}

    @Form(
        name = "EditInternalNote",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEvent",
        defaultMapName = "communicationEvent",
        fields = {
            @FormField(name = "form", hidden = @HiddenField),
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "communicationEventTypeId", hidden = @HiddenField(value = "COMMENT_NOTE")),
            @FormField(name = "parentCommEventId", hidden = @HiddenField(value = "${parameters.parentCommEventId}")),
            @FormField(name = "statusId", hidden = @HiddenField(value = "COM_PENDING")),
            @FormField(name = "datetimeStarted", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "my", hidden = @HiddenField(value = "${parameters.my}")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "recentParties", keyName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}"))),
            @FormField(name = "subject", text = @TextField(size = 60)),
            @FormField(name = "contentMimeTypeId", hidden = @HiddenField(value = "text/plain")),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", encodeOutput = false, textarea = @TextareaField(cols = 72, rows = 15, defaultValue = "${parameters.content}")),
            @FormField(name = "sendAction", title = " ", useWhen = "communicationEvent!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_send}", hyperlink = @HyperlinkField(target = "javascript:(document.EditInternalNote.form.value='list'),(document.EditInternalNote.statusId.value='COM_ENTERED'),(document.EditInternalNote.datetimeStarted.value='${nowDate}'),(document.EditInternalNote.submit())", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSend}")),
            @FormField(name = "saveAction", title = " ", useWhen = "communicationEvent!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", position = 2, hyperlink = @HyperlinkField(target = "javascript:(document.EditInternalNote.form.value='list'),(document.EditInternalNote.submit())", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSave}")),
            @FormField(name = "createAction", useWhen = "communicationEvent==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "communicationEvent!=null", target = "updateCommunicationEvent")
        },
        actions = @FormActions(set = {@SetAction(field = "nowDate", value = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"yyyy-MM-dd HH:mm:ss.S\")}", type = "String")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/communication/recentVisitor.groovy")})
    )
    public interface EditInternalNote {}

    @Form(
        name = "ViewEmail",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        defaultMapName = "communicationEvent",
        fields = {
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${communicationEvent.communicationEventId}", parameters = {@ParameterDef(paramName = "communicationEventId", fromField = "communicationEvent.communicationEventId")})),
            @FormField(name = "communicationEventTypeId", displayEntity = @DisplayEntityField(entityName = "CommunicationEventType", description = "${description}")),
            @FormField(name = "contactListId", useWhen = "communicationEvent.get(\"contactListId\")!=null", display = @DisplayField),
            @FormField(name = "partyIdFrom", useWhen = "communicationEvent.get(\"partyIdFrom\")!=null", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} (${contactMechFrom.infoString})", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${communicationEvent.partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "communicationEvent.partyIdFrom")}))),
            @FormField(name = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} (${contactMechTo.infoString})", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${communicationEvent.partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "communicationEvent.partyIdTo")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "lastUpdatedStamp", title = "${uiLabelMap.FormFieldTitle_lastModifiedDate}", display = @DisplayField),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonSendDate}", display = @DisplayField(type = "date")),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonEndDate}", display = @DisplayField(type = "date")),
            @FormField(name = "subject", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "parentCommEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${communicationEvent.parentCommEventId}", parameters = {@ParameterDef(paramName = "communicationEventId", fromField = "communicationEvent.parentCommEventId")})),
            @FormField(name = "note", title = "${uiLabelMap.CommonNote}", display = @DisplayField)
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ContactMech", valueField = "contactMechFrom"), @EntityOneAction(entityName = "ContactMech", valueField = "contactMechTo")}),
        sortOrder = @SortOrder()
    )
    public interface ViewEmail {}

    @Form(
        name = "ViewCommEvent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        defaultMapName = "communicationEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", display = @DisplayField),
            @FormField(name = "contactListId", useWhen = "contactListId!=null", displayEntity = @DisplayEntityField(entityName = "ContactList", description = "${contactListName}", subHyperlink = @SubHyperlink(target = "/marketing/control/EditContactList?contactListId=${communicationEvent.contactListId}", description = "[${communicationEvent.contactListId}]"))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${communicationEvent.partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "communicationEvent.partyIdTo")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "contactMechIdFrom", title = "${uiLabelMap.PartyFromContactMech}", useWhen = "\"my\"==void", display = @DisplayField),
            @FormField(name = "contactMechIdTo", title = "${uiLabelMap.PartyToEmailAddress}", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "ContactMech", keyFieldName = "contactMechId", description = "${infoString}")),
            @FormField(name = "roleTypeIdFrom", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", useWhen = "\"my\"==void", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField),
            @FormField(name = "subject", display = @DisplayField),
            @FormField(name = "eventNote", mapName = "subjectMap", title = "${uiLabelMap.CommonNote}", display = @DisplayField),
            @FormField(name = "contentMimeTypeId", useWhen = "\"my\"==void", display = @DisplayField),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", encodeOutput = false, textarea = @TextareaField(rows = 10, readonly = true))
        },
        sortOrder = @SortOrder()
    )
    public interface ViewCommEvent {}

    @Form(
        name = "FindCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "FindCommunicationEvents",
        focusFieldName = "submitAction",
        paginate = "true",
        headerRowStyle = "header-row",
        defaultPositionSpan = 1,
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "communicationEventId", textFind = @TextFindField),
            @FormField(name = "parentCommEventId", position = 2, textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonTo}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.PartyAnyRole}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "COMMEVENT_ROLE")}))),
            @FormField(name = "communicationEventTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CommunicationEventType", description = "${description}", keyFieldName = "communicationEventTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "subject", mapName = "subjectMap", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCommEvents {}

    @Form(
        name = "ListCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        listName = "commEvents",
        paginate = "true",
        headerRowStyle = "header-row",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${subject} [${communicationEventId}]", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "activeSubMenuItem", fromField = "parameters.activeSubMenuItem")})),
            @FormField(name = "communicationEventTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "CommunicationEventType", keyFieldName = "communicationEventTypeId", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonPartyId}", sortField = true, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "roleTypeId", sortField = true, displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "roleStatusId", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "entryDate", title = "${uiLabelMap.CommonCreated}", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonSent}", sortField = true, display = @DisplayField(type = "date"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "subject", fromField = "subject", defaultValue = "${uiLabelMap.PartyNoSubject}")})
    )
    public interface ListCommEvents {}

    @Form(
        name = "FindCommunicationByOrder",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "FindCommunicationByOrder",
        focusFieldName = "submitAction",
        paginate = "true",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "orderId", textFind = @TextFindField),
            @FormField(name = "communicationEventId", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonTo}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.PartyAnyRole}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "communicationEventTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CommunicationEventType", description = "${description}", keyFieldName = "communicationEventTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCommunicationByOrder {}

    @Form(
        name = "ListCommunicationByOrder",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        extendsForm = "ListCommEvents",
        headerRowStyle = "header-row-2",
        fields = {
            @FormField(name = "orderId", title = "${uiLabelMap.FormFieldTitle_orderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview?orderId=${orderId}", urlMode = UrlMode.INTER_APP, description = "${orderId}")),
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${communicationEventId}", parameters = {@ParameterDef(paramName = "communicationEventId")})),
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${subject}", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", value = "-entryDate"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListCommunicationByOrder {}

    @Form(
        name = "ListPartyCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "RemoveCommunicationEventRole",
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        sortFieldParameterName = "partyCommEventSortField",
        fields = {
            @FormField(name = "deleteCommEventIfLast", hidden = @HiddenField(value = "Y")),
            @FormField(name = "delContentDataResource", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_text}", widgetAreaStyle = "fieldWidth300", sortField = true, hyperlink = @HyperlinkField(target = "ViewCommunicationEvent", description = "${subject}[${communicationEventId}] ", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "my"), @ParameterDef(paramName = "form", value = "view"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonFrom}", sortField = true, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonPartyId} ${uiLabelMap.CommonTo}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyIdTo}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "entryDate", sortField = true, display = @DisplayField(type = "date-time")),
            @FormField(name = "communicationEventTypeId", displayEntity = @DisplayEntityField(entityName = "CommunicationEventType", description = "${description}")),
            @FormField(name = "statusId", entryName = "roleStatusId", title = "${uiLabelMap.CommonStatus}", widgetStyle = "${styles.link_nav_info_desc}", widgetAreaStyle = "fieldWidth300", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}", subHyperlink = @SubHyperlink(target = "setCommunicationEventRoleStatus", description = "${uiLabelMap[toComplete]}", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "statusId", value = "COM_ROLE_COMPLETED"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId")}))),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.partyCommEventSortField", fromField = "parameters.partyCommEventSortField", defaultValue = "${groovy: 'true'.equals(parameters.all) ? '-entryDate' : 'entryDate'}"), @SetAction(field = "searchParameters.partyId", fromField = "partyId"), @SetAction(field = "searchParameters.statusId", value = "COM_UNKNOWN_PARTY"), @SetAction(field = "searchParameters.statusId_op", value = "notEqual"), @SetAction(field = "searchParameters.roleStatusId", value = "${groovy: 'true'.equals(parameters.all) ? 'dummy' : 'COM_ROLE_COMPLETED'}"), @SetAction(field = "searchParameters.roleStatusId_op", value = "notEqual"), @SetAction(field = "searchParameters.communicationEventTypeId", value = "${groovy: context.internalNotesOnly && 'true'.equals(context.internalNotesOnly) ? 'COMMENT_NOTE' : ''}")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "searchParameters"), @FieldMap(fieldName = "entityName", value = "CommunicationEventAndRole"), @FieldMap(fieldName = "orderBy", fromField = "parameters.partyCommEventSortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "toComplete", value = "${groovy: 'COM_ROLE_READ'.equals(roleStatusId) ? 'PartyToComplete' : ' '}")})
    )
    public interface ListPartyCommEvents {}

    @Form(
        name = "ListPendingCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        extendsForm = "ListCommEvents",
        oddRowStyle = "alternate-row"
    )
    public interface ListPendingCommEvents {}

    @Form(
        name = "ListUnknownPartyEmails",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.MULTI,
        target = "deleteCommunicationEvents",
        extendsForm = "ListCommEvents",
        headerRowStyle = "header-row-2",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "delContentDataResource", hidden = @HiddenField(value = "Y")),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonYes}", check = @CheckField),
            @FormField(name = "deleteSelectedAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-entryDate")})
    )
    public interface ListUnknownPartyEmails {}

    @Form(
        name = "ListChildCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        extendsForm = "ListCommEvents",
        paginateTarget = "EditCommunicationEvent"
    )
    public interface ListChildCommEvents {}

    @Form(
        name = "ListLookupCommEvents",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "deleteCommunicationEvent",
        listName = "listIt",
        extendsForm = "ListCommEvents",
        headerRowStyle = "header-row-2",
        fields = {
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-entryDate")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupCommEvents {}

    @Form(
        name = "ListCommWorkEfforts",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        paginateTarget = "ListCommWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "/workeffort/control/WorkEffortSummary", description = "${workEffortId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId")})))
        }
    )
    public interface ListCommWorkEfforts {}

    @Form(
        name = "AddCommEventWorkEffort",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "/partymgr/control/createCommEventWorkEffort",
        extendsForm = "EditWorkEffort",
        extendsResource = "component://workeffort/widget/WorkEffortForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", display = @DisplayField),
            @FormField(name = "workEffortId", useWhen = "workEffort==null&&workEffortId==null", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "submitButton", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "communicationEventId"), @SortField(name = "workEffortId")})
    )
    public interface AddCommEventWorkEffort {}

    @Form(
        name = "ListCommPurposes",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        paginateTarget = "UpdateCommPurposes",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventPrpTypId", displayEntity = @DisplayEntityField(entityName = "CommunicationEventPrpTyp", description = "${description}")),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeCommunicationEventPurpose", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "communicationEventPrpTypId"), @ParameterDef(paramName = "communicationEventId")}))
        }
    )
    public interface ListCommPurposes {}

    @Form(
        name = "AddEventPurpose",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEventPurpose",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "communicationEventPrpTypId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CommunicationEventPrpTyp", description = "${description}", keyFieldName = "communicationEventPrpTypId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.PartyAddPurpose}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEventPurpose {}

    @Form(
        name = "ListCommRoles",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "RemoveCommunicationEventRole",
        paginateTarget = "UpdateCommRoles",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "contactMechId", displayEntity = @DisplayEntityField(entityName = "ContactMech", description = "${infoString}")),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListCommRoles {}

    @Form(
        name = "ListCommRolesInline",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        extendsForm = "ListCommRoles",
        paginate = "false",
        hideTableWhen = "${!formHasListResult}",
        useAlternateTextWhen = "${false}"
    )
    public interface ListCommRolesInline {}

    @Form(
        name = "ViewCommRoles",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        extendsForm = "ListCommRoles",
        paginate = "false",
        fields = {
            @FormField(name = "removeAction", ignored = @IgnoredField)
        }
    )
    public interface ViewCommRoles {}

    @Form(
        name = "AddEventRole",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEventRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "subject", hidden = @HiddenField),
            @FormField(name = "content", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "datetimeStarted", hidden = @HiddenField),
            @FormField(name = "my", hidden = @HiddenField(value = "${my}")),
            @FormField(name = "datetimeStarted", hidden = @HiddenField),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", constraints = {@EntityConstraint(name = "parentTypeId", value = "COMMEVENT_ROLE"), @EntityConstraint(name = "roleTypeId", value = "ORIGINATOR", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "${AddEventRole_submitAction}", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.PartyAddRole}"))
        },
        actions = @FormActions(set = {@SetAction(field = "AddEventRole_submitAction", fromField = "AddEventRole_submitAction", defaultValue = "javascript:document.AddEventRole.submit()")})
    )
    public interface AddEventRole {}

    @Form(
        name = "listCommContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "removeAttachFile",
        listName = "contentDataResourceList",
        paginate = "false",
        oddRowStyle = "alternate-row",
        hideTableWhen = "${!formHasListResult}",
        useAlternateTextWhen = "${false}",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "fromDate", hidden = @HiddenField),
            @FormField(name = "my", hidden = @HiddenField),
            @FormField(name = "contentName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewSimpleContent", description = "${contentName} [${contentId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface listCommContent {}

    @Form(
        name = "addCommContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommContentDataResource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "contentId", ignored = @IgnoredField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "DOCUMENT")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "drMimeTypeId", widgetStyle = "+smallSelect", dropDown = @DropDownField(options = {@Option(key = "application/msword", description = "${uiLabelMap.ContentMSWord}"), @Option(key = "application/pdf", description = "${uiLabelMap.ContentPDFFile}"), @Option(key = "application/vnd.ofbiz.survey", description = "${uiLabelMap.ContentSurvey}"), @Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "application/octet-stream", description = "${uiLabelMap.ContentResourceOther}")})),
            @FormField(name = "drIsPublic", title = "${uiLabelMap.PartyIsPublic}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface addCommContent {}

    @Form(
        name = "editCommContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "updateCommContentDataResource",
        defaultMapName = "commEventContentDataResource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", display = @DisplayField),
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "application/msword", description = "${uiLabelMap.ContentMSWord}"), @Option(key = "application/pdf", description = "${uiLabelMap.ContentPDFFile}"), @Option(key = "application/vnd.ofbiz.survey", description = "${uiLabelMap.ContentSurvey}"), @Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "application/octet-stream", description = "${uiLabelMap.ContentResourceOther}")})),
            @FormField(name = "drIsPublic", title = "${uiLabelMap.PartyIsPublic}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "dataResourceTypeId", entryName = "drDataResourceTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface editCommContent {}

    @Form(
        name = "uploadCommContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.UPLOAD,
        target = "uploadCommEventContent",
        defaultMapName = "commEventContentDataResource",
        fields = {
            @FormField(name = "contentId", mapName = "contentAssoc", hidden = @HiddenField),
            @FormField(name = "drDataResourceId", hidden = @HiddenField),
            @FormField(name = "drDataResourceTypeId", hidden = @HiddenField),
            @FormField(name = "drMimeTypeId", hidden = @HiddenField),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "fromDate", hidden = @HiddenField),
            @FormField(name = "drObjectInfo", text = @TextField),
            @FormField(name = "imageData", mapName = "emptyMap", file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface uploadCommContent {}

    @Form(
        name = "uploadContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.UPLOAD,
        target = "uploadAttachFiletoEmail",
        attribs = "{'showProgress':'${showProgress}', 'progressSuccessAction':'${progressSuccessAction}', 'progressOptions':${groovy: context.progressOptions ?: \"''\"} }",
        fields = {
            @FormField(name = "dataCategoryId", hidden = @HiddenField(value = "PERSONAL")),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "DOCUMENT")),
            @FormField(name = "resourceStatusId", hidden = @HiddenField(value = "CTNT_PUBLISHED")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyIdFrom}")),
            @FormField(name = "partyContentTypeId", hidden = @HiddenField(value = "USERDEF")),
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "CONTENT")),
            @FormField(name = "communicationEventId", hidden = @HiddenField(value = "${communicationEvent.communicationEventId}")),
            @FormField(name = "communicationEventTypeId", hidden = @HiddenField(value = "${communicationEvent.communicationEventTypeId}")),
            @FormField(name = "parentCommEventId", hidden = @HiddenField(value = "${communicationEvent.parentCommEventId}")),
            @FormField(name = "origCommEventId", hidden = @HiddenField(value = "${communicationEvent.origCommEventId}")),
            @FormField(name = "subject", hidden = @HiddenField),
            @FormField(name = "content", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "datetimeStarted", hidden = @HiddenField),
            @FormField(name = "my", hidden = @HiddenField(value = "${my}")),
            @FormField(name = "contentId", title = "${uiLabelMap.FormFieldTitle_existContentId}", lookup = @LookupField(targetFormName = "LookupTreeContent")),
            @FormField(name = "uploadedFile", file = @FileField),
            @FormField(name = "contentIdFrom", title = "${uiLabelMap.ContentCompDocParentContentId}", lookup = @LookupField(targetFormName = "LookupDetailContentTree")),
            @FormField(name = "sendAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", hyperlink = @HyperlinkField(target = "${uploadContent_submitAction}", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonUpload}")),
            @FormField(name = "submitProgress", title = " ", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "uploadContent_submitAction", fromField = "uploadContent_submitAction", defaultValue = "javascript:(document.uploadContent.datetimeStarted.value=document.EditEmail.datetimeStarted.value),(document.uploadContent.partyIdTo.value=document.EditEmail.partyIdTo.value),(document.uploadContent.subject.value=document.EditEmail.subject.value),(document.uploadContent.content.value=document.EditEmail.content.value),(jQuery(document.uploadContent).trigger('submit')),void(0)")})
    )
    public interface uploadContent {}

    @Form(
        name = "uploadContent1",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.UPLOAD,
        target = "uploadAttachFile",
        extendsForm = "uploadContent",
        fields = {
            @FormField(name = "send", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface uploadContent1 {}

    @Form(
        name = "editCommTextContent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "uploadCommEventContent",
        defaultMapName = "commEventContentDataResource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "drDataResourceId", hidden = @HiddenField),
            @FormField(name = "drDataResourceTypeId", hidden = @HiddenField),
            @FormField(name = "drMimeTypeId", hidden = @HiddenField),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "fromDate", hidden = @HiddenField),
            @FormField(name = "textData", mapName = "electronicText", title = "${uiLabelMap.FormFieldTitle_textDataTitle}", textarea = @TextareaField(rows = 30)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface editCommTextContent {}

    @Form(
        name = "ListMyUnknownPartyEmails",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.MULTI,
        target = "deleteCommunicationEvents",
        title = "Email List unknown parties",
        listName = "commEventsUnknown",
        defaultEntityName = "CommunicationEvent",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "delContentDataResource", hidden = @HiddenField(value = "Y")),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_text}", widgetAreaStyle = "fieldWidth300", hyperlink = @HyperlinkField(target = "${parameters._LAST_VIEW_NAME_}", description = "${subject}", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "my", value = "My"), @ParameterDef(paramName = "form", value = "view"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @FormField(name = "entryDate", display = @DisplayField(type = "date")),
            @FormField(name = "note", widgetAreaStyle = "fieldWidth200", display = @DisplayField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonYes}", check = @CheckField),
            @FormField(name = "deleteSelectedAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "subject", fromField = "subject", defaultValue = "${uiLabelMap.PartyNoSubject}")})
    )
    public interface ListMyUnknownPartyEmails {}

    @Form(
        name = "allocateMsgToPartyForm",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "allocateMsgToParty",
        title = "create a new party for a unknown incoming email address",
        fields = {
            @FormField(name = "form", hidden = @HiddenField(value = "list")),
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "donePage", hidden = @HiddenField(value = "${donePage}")),
            @FormField(name = "communicationEventId", hidden = @HiddenField(value = "${parameters.communicationEventId}")),
            @FormField(name = "partyId", entryName = "dummy", tooltip = "${uiLabelMap.PartyLeaveEmpty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "emailAddress", text = @TextField),
            @FormField(name = "firstName", text = @TextField),
            @FormField(name = "middleName", position = 2, text = @TextField),
            @FormField(name = "lastName", text = @TextField),
            @FormField(name = "submit", title = "${uiLabelMap.PartyCreateAddEmail}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/communication/getPartyEmailFromCommEventInfo.groovy")})
    )
    public interface allocateMsgToPartyForm {}

    @Form(
        name = "deleteEmail",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "deleteUnknownCommunicationEvent",
        title = "delete the email",
        fields = {
            @FormField(name = "donePage", hidden = @HiddenField(value = "${donePage}")),
            @FormField(name = "communicationEventId", hidden = @HiddenField(value = "${parameters.communicationEventId}")),
            @FormField(name = "delContentDataResource", hidden = @HiddenField(value = "Y")),
            @FormField(name = "dummy1", title = " ", display = @DisplayField),
            @FormField(name = "dummy2", title = " ", position = 2, display = @DisplayField),
            @FormField(name = "deleteEmail", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", position = 3, submit = @SubmitField)
        }
    )
    public interface deleteEmail {}

    @Form(
        name = "EditRequestFromCommEvent",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createRequestFromCommEvent",
        defaultMapName = "communicationEvent",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField(value = "${parameters.communicationEventId}")),
            @FormField(name = "fromPartyId", entryName = "partyIdFrom", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "custRequestTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CustRequestType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "custRequestName", entryName = "subject", text = @TextField(size = 60)),
            @FormField(name = "contentMimeTypeId", hidden = @HiddenField),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", textarea = @TextareaField(cols = 70, rows = 20)),
            @FormField(name = "submit", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditRequestFromCommEvent {}

    @Form(
        name = "ListRequests",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        extendsForm = "ListRequests",
        extendsResource = "component://order/widget/ordermgr/CustRequestForms.xml",
        fields = {
            @FormField(name = "custRequestName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/ordermgr/control/ViewRequest", urlMode = UrlMode.INTER_APP, description = "${custRequestName} [${custRequestId}]", parameters = {@ParameterDef(paramName = "custRequestId")}))
        }
    )
    public interface ListRequests {}

    @Form(
        name = "EditCommPortletParams",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "communicationPartyId", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${userLogin.partyId}")),
            @FormField(name = "internalNotesOnly", dropDown = @DropDownField(options = {@Option(key = "false", description = "${uiLabelMap.CommonFalse}"), @Option(key = "true", description = "${uiLabelMap.CommonTrue}")})),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCommPortletParams {}

    @Form(
        name = "ListDraftEmails",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "deleteCommunicationEvent",
        listName = "commEventDraft",
        extendsForm = "ListCommEvents",
        extendsResource = "component://party/widget/partymgr/CommunicationEventForms.xml",
        headerRowStyle = "header-row-2",
        useRowSubmit = true,
        fields = {
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "${parameters._LAST_VIEW_NAME_}", description = "${subject}", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "my"), @ParameterDef(paramName = "form", value = "edit"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId")})),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListDraftEmails {}

    @Form(
        name = "ListProgressEmails",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        listName = "commEventProgress",
        extendsForm = "ListPartyCommEvents",
        fields = {
            @FormField(name = "partyIdFrom", ignored = @IgnoredField),
            @FormField(name = "startDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListProgressEmails {}

    @Form(
        name = "ListCommOrders",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "deleteCommunicationEventOrder",
        listName = "ordersList",
        paginateTarget = "UpdateCommOrders",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "orderId", title = "${uiLabelMap.FormFieldTitle_orderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview?orderId=${orderId}", urlMode = UrlMode.INTER_APP, description = "${orderId}")),
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "orderTypeId", title = "${uiLabelMap.OrderOrderType}", displayEntity = @DisplayEntityField(entityName = "OrderType", keyFieldName = "orderTypeId", description = "${description}")),
            @FormField(name = "createdBy", title = "${uiLabelMap.CommonCreatedBy}", display = @DisplayField(description = "${orderHeader.createdBy}")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "orderTypeId", fromField = "orderHeader.orderTypeId")}, entityOne = {@EntityOneAction(entityName = "OrderHeader", valueField = "orderHeader")})
    )
    public interface ListCommOrders {}

    @Form(
        name = "AddCommOrder",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEventOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PartyOrderAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCommOrder {}

    @Form(
        name = "ListCommProducts",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        type = FormType.LIST,
        target = "deleteCommunicationEventProduct",
        listName = "productsList",
        paginateTarget = "ListCommProducts",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.PartyProductId}", display = @DisplayField),
            @FormField(name = "internalName", display = @DisplayField(description = "${product.internalName}")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product")})
    )
    public interface ListCommProducts {}

    @Form(
        name = "AddCommProduct",
        location = "component://party/widget/partymgr/CommunicationEventForms.xml",
        target = "createCommunicationEventProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.PartyProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PartyProductAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCommProduct {}

}
