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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class MarketingReportScreens {

    @Screen(name = "CommonMarketReportDecorator", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Reports")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonMarketReportDecorator {}

    @Screen(name = "MarketingReportList", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingReports")
    @DecoratorScreen(
        name = "CommonMarketReportDecorator",
        location = "component://marketing/widget/MarketingReportScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "h2", labels = {
                    @Label(text = "${uiLabelMap.MarketingReports}")}),
                    @Container(style = "${styles.grid_large}6", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.MarketingTrackingCodeReportTitle}", includeForms = {
                    @IncludeForm(name = "TrackingCodeReportOptions", location = "component://marketing/widget/ReportForms.xml"
                
                    )}),
                    @ScreenletNested(title = "${uiLabelMap.MarketingEmailStatusReport}", includeForms = {
                    @IncludeForm(name = "EmailStatusOptions", location = "component://marketing/widget/ReportForms.xml"
                
                )})}),
                @Container(style = "${styles.grid_large}6", screenlets = {
                    @ScreenletNested(title = "${uiLabelMap.MarketingCampaignReportTitle}", includeForms = {
                    @IncludeForm(name = "MarketingCampaignOptions", location = "component://marketing/widget/ReportForms.xml"
                
                )}),
                @ScreenletNested(title = "${uiLabelMap.MarketingPartyStatusReport}", includeForms = {
                    @IncludeForm(name = "PartyStatusOptions", location = "component://marketing/widget/ReportForms.xml"
                
            )})})})
        }
    )
    public interface MarketingReportList {}

    @Screen(name = "TrackingCodeReport", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingTrackingCodeReportTitle")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "fromDate", fromField = "requestParameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "requestParameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "requestParameters.trackingCodeId")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/reports/TrackingCodeReport.groovy")
    @DecoratorScreen(
        name = "CommonMarketReportDecorator",
        location = "component://marketing/widget/MarketingReportScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingTrackingCodeReportTitle} ${uiLabelMap.CommonFrom} ${parameters.fromDate} ${uiLabelMap.CommonThru} ${parameters.thruDate}", includeForms = {
                    @IncludeForm(name = "TrackingCodeReport", location = "component://marketing/widget/ReportForms.xml"
                )})})
        }
    )
    public interface TrackingCodeReport {}

    @Screen(name = "MarketingCampaignReport", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingTrackingCodeReportTitle}")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "fromDate", fromField = "requestParameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "requestParameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/reports/MarketingCampaignReport.groovy")
    @DecoratorScreen(
        name = "CommonMarketReportDecorator",
        location = "component://marketing/widget/MarketingReportScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingCampaignReportTitle} ${uiLabelMap.CommonFrom} ${parameters.fromDate} ${uiLabelMap.CommonThru} ${parameters.thruDate}", includeForms = {
                    @IncludeForm(name = "MarketCampaignReport", location = "component://marketing/widget/ReportForms.xml"
                )})})
        }
    )
    public interface MarketingCampaignReport {}

    @Screen(name = "EmailStatusReport", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingEmailStatusReport}")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "statusId", fromField = "parameters.statusId", valueType = "String")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom", valueType = "String")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo", valueType = "String")
    @Action(type = ActionType.SET, field = "roleStatusId", fromField = "parameters.roleStatusId", valueType = "String")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/reports/EmailStatusReport.groovy")
    @DecoratorScreen(
        name = "CommonMarketReportDecorator",
        location = "component://marketing/widget/MarketingReportScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingEmailStatusReport}", includeForms = {
                    @IncludeForm(name = "EmailStatusReport", location = "component://marketing/widget/ReportForms.xml"
                )})})
        }
    )
    public interface EmailStatusReport {}

    @Screen(name = "PartyStatusReport", location = "component://marketing/widget/MarketingReportScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingPartyStatusReport}")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "statusDate", fromField = "parameters.statusDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId", valueType = "String")
    @Action(type = ActionType.SET, field = "statusId", fromField = "parameters.statusId", valueType = "String")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/reports/PartyStatusReport.groovy")
    @DecoratorScreen(
        name = "CommonMarketReportDecorator",
        location = "component://marketing/widget/MarketingReportScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingPartyStatusReport}", includeForms = {
                    @IncludeForm(name = "PartyStatusReport", location = "component://marketing/widget/ReportForms.xml"
                )})})
        }
    )
    public interface PartyStatusReport {}

}
