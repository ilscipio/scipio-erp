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
package com.ilscipio.scipio.marketing.widget;

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
public class ReportForms {

    @Form(
        name = "TrackingCodeReportOptions",
        location = "component://marketing/widget/ReportForms.xml",
        target = "TrackingCodeReport",
        title = "${uiLabelMap.MarketingTrackingCodeReportTitle}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom} (${uiLabelMap.CommonDate}>=)", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru} (${uiLabelMap.CommonDate}<)", dateTime = @DateTimeField),
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCode}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "TrackingCode", description = "${trackingCodeId}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun} ${uiLabelMap.MarketingTrackingCodeReportTitle}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface TrackingCodeReportOptions {}

    @Form(
        name = "MarketingCampaignOptions",
        location = "component://marketing/widget/ReportForms.xml",
        target = "MarketingCampaignReport",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom} (${uiLabelMap.CommonDate}>=)", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru} (${uiLabelMap.CommonDate}<)", dateTime = @DateTimeField),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaign}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun} ${uiLabelMap.MarketingCampaignReportTitle}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface MarketingCampaignOptions {}

    @Form(
        name = "EmailStatusOptions",
        location = "component://marketing/widget/ReportForms.xml",
        target = "EmailStatusReport",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom} (${uiLabelMap.CommonDate}>=)", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru} (${uiLabelMap.CommonDate}<)", dateTime = @DateTimeField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.FormFieldTitle_partyIdTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.FormFieldTitle_fromPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "roleStatusId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupRole} ${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_ROL_STATUS")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun} ${uiLabelMap.MarketingEmailStatusReport}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface EmailStatusOptions {}

    @Form(
        name = "PartyStatusOptions",
        location = "component://marketing/widget/ReportForms.xml",
        target = "PartyStatusReport",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom} (${uiLabelMap.CommonDate}>=)", dateTime = @DateTimeField),
            @FormField(name = "statusDate", title = "${uiLabelMap.CommonStatus} (${uiLabelMap.CommonDate}>=)", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru} (${uiLabelMap.CommonDate}<)", dateTime = @DateTimeField),
            @FormField(name = "contactListId", title = "${uiLabelMap.PartyContactLists}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "ContactList", description = "${contactListName}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun} ${uiLabelMap.MarketingPartyStatusReport}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface PartyStatusOptions {}

    @Form(
        name = "TrackingCodeReport",
        location = "component://marketing/widget/ReportForms.xml",
        type = FormType.LIST,
        title = "${uiLabelMap.MarketingTrackingCodeReportTitle}",
        listName = "trackingCodeVisitAndOrders",
        paginateTarget = "TrackingCodeReport",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCode}", displayEntity = @DisplayEntityField(entityName = "TrackingCode", keyFieldName = "trackingCodeId", description = "${description} [${trackingCodeId}]")),
            @FormField(name = "visits", title = "${uiLabelMap.MarketingVisits}", display = @DisplayField),
            @FormField(name = "orders", title = "${uiLabelMap.MarketingOrders}", display = @DisplayField),
            @FormField(name = "orderAmount", title = "${uiLabelMap.MarketingOrderAmount}", display = @DisplayField),
            @FormField(name = "conversionRate", title = "${uiLabelMap.MarketingConversionRate}", display = @DisplayField)
        }
    )
    public interface TrackingCodeReport {}

    @Form(
        name = "MarketCampaignReport",
        location = "component://marketing/widget/ReportForms.xml",
        type = FormType.LIST,
        title = "${uiLabelMap.MarketingCampaignReportTitle}",
        listName = "marketingCampaignVisitAndOrders",
        paginateTarget = "MarketCampaignReport",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignName}", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", keyFieldName = "marketingCampaignId", description = "${campaignName} [${marketingCampaignId}]")),
            @FormField(name = "visits", title = "${uiLabelMap.MarketingVisits}", display = @DisplayField),
            @FormField(name = "orders", title = "${uiLabelMap.MarketingOrders}", display = @DisplayField),
            @FormField(name = "orderAmount", title = "${uiLabelMap.MarketingOrderAmount}", display = @DisplayField),
            @FormField(name = "conversionRate", title = "${uiLabelMap.MarketingConversionRate}", display = @DisplayField)
        }
    )
    public interface MarketCampaignReport {}

    @Form(
        name = "EmailStatusReport",
        location = "component://marketing/widget/ReportForms.xml",
        type = FormType.LIST,
        title = "${uiLabelMap.MarketingEmailStatusReport}",
        listName = "commStatausList",
        paginateTarget = "EmailStatusReport",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", display = @DisplayField),
            @FormField(name = "communicationEventTypeId", title = "${uiLabelMap.MarketingContactListCommEventTypeId}", display = @DisplayField(description = "${communicationEventType.description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.FormFieldTitle_toPartyId}", display = @DisplayField),
            @FormField(name = "roleStatusId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupRole} ${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.FormFieldTitle_fromPartyId}", display = @DisplayField),
            @FormField(name = "entryDate", title = "${uiLabelMap.PartyEnteredDate}", display = @DisplayField),
            @FormField(name = "subject", title = "${uiLabelMap.PartySubject}", display = @DisplayField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "CommunicationEventType", valueField = "communicationEventType")})
    )
    public interface EmailStatusReport {}

    @Form(
        name = "PartyStatusReport",
        location = "component://marketing/widget/ReportForms.xml",
        type = FormType.LIST,
        title = "${uiLabelMap.MarketingPartyStatusReport}",
        listName = "partyStatusLists",
        paginateTarget = "PartyStatusReport",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactList}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "statusDate", title = "${uiLabelMap.CommonStatus} ${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", display = @DisplayField(description = "${statusItem.description}"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "statusItem")})
    )
    public interface PartyStatusReport {}

}
