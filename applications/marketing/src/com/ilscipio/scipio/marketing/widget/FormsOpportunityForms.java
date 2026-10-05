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
public class FormsOpportunityForms {

    @Form(
        name = "FindSalesOpportunity",
        location = "component://marketing/widget/sfa/forms/OpportunityForms.xml",
        target = "FindSalesOpportunity",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "salesOpportunityId", hidden = @HiddenField),
            @FormField(name = "opportunityName", title = "${uiLabelMap.SfaFindOpportunities}", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.SfaLead}", lookup = @LookupField(targetFormName = "LookupLeads")),
            @FormField(name = "opportunityStageId", title = "${uiLabelMap.SfaInitialStage}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SalesOpportunityStage", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "sequenceNum")}))),
            @FormField(name = "typeEnumId", title = "${uiLabelMap.SfaType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "SLSOPP_TYP_ENUM")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSalesOpportunity {}

    @Form(
        name = "ListSalesOpportunity",
        location = "component://marketing/widget/sfa/forms/OpportunityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindSalesOpportunity",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "salesOpportunityId", hidden = @HiddenField),
            @FormField(name = "opportunityName", title = "${uiLabelMap.SfaOpportunityName}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditSalesOpportunity", description = "${opportunityName} [${salesOpportunityId}] ${roleTypeId}", parameters = {@ParameterDef(paramName = "salesOpportunityId")})),
            @FormField(name = "opportunityStageId", title = "${uiLabelMap.SfaInitialStage}", displayEntity = @DisplayEntityField(entityName = "SalesOpportunityStage", description = "${description}")),
            @FormField(name = "partyId", title = "${uiLabelMap.SfaLead}/${uiLabelMap.Account}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${middleName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "nextStep", display = @DisplayField),
            @FormField(name = "estimatedAmount", title = "${uiLabelMap.SfaEstimatedAmount}", display = @DisplayField),
            @FormField(name = "nextStepDate", title = "${uiLabelMap.SfaNextStepDate}", sortField = true, display = @DisplayField),
            @FormField(name = "estimatedCloseDate", title = "${uiLabelMap.SfaCloseDate}", display = @DisplayField),
            @FormField(name = "editAction", title = "${uiLabelMap.CommonClose}", useWhen = "estimatedCloseDate == void || estimatedCloseDate == null || org.ofbiz.base.util.UtilValidate.isDateAfterNow(estimatedCloseDate) || opportunityStageId != \"SOSTG_CLOSED\"", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "closeSalesOpportunity", description = "${uiLabelMap.CommonClose}", parameters = {@ParameterDef(paramName = "salesOpportunityId"), @ParameterDef(paramName = "opportunityStageId", value = "SOSTG_CLOSED")}))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.noConditionFind", value = "Y"), @SetAction(field = "opportunityStageId", fromField = "parameters.opportunityStageId"), @SetAction(field = "parameters.opportunityStageId", value = "${groovy:opportunityStageId==null?'SOSTG_CLOSED':opportunityStageId}"), @SetAction(field = "parameters.opportunityStageId_op", value = "${groovy:opportunityStageId==null?'notEqual':'equals'}"), @SetAction(field = "fieldList", value = "${groovy:['partyId','salesOpportunityId','opportunityStageId','typeEnumId', 'roleTypeId']}", type = "List"), @SetAction(field = "sortField", fromField = "parameters.sortField", defaultValue = "salesOpportunityId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SalesOpportunityAndRole"), @FieldMap(fieldName = "orderBy", value = "${sortField}"), @FieldMap(fieldName = "fieldList", fromField = "fieldList"), @FieldMap(fieldName = "distinct", value = "Y"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "SalesOpportunity", valueField = "salesOpportunity")})
    )
    public interface ListSalesOpportunity {}

    @Form(
        name = "EditSalesOpportunity",
        location = "component://marketing/widget/sfa/forms/OpportunityForms.xml",
        target = "updateSalesOpportunity",
        defaultMapName = "salesOpportunity",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "salesOpportunityId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "salesOpportunity!=null", display = @DisplayField),
            @FormField(name = "salesOpportunityId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${salesOpportunityId}]", useWhen = "salesOpportunity==null&&salesOpportunityId!=null", display = @DisplayField),
            @FormField(name = "opportunityName", title = "${uiLabelMap.SfaOpportunityName}", useWhen = "communicationEvent!=null && communicationEvent.subject!=null", text = @TextField(size = 30, defaultValue = "${communicationEvent.subject}")),
            @FormField(name = "opportunityName", useWhen = "communicationEvent==null", text = @TextField(size = 30, defaultValue = "${uiLabelMap.SfaOpportunity}")),
            @FormField(name = "description", title = "${uiLabelMap.MarketingSegmentGroupDescription}", textarea = @TextareaField(defaultValue = "${communicationEvent.content}")),
            @FormField(name = "nextStep", title = "${uiLabelMap.SfaNextStep}", textarea = @TextareaField),
            @FormField(name = "estimatedAmount", title = "${uiLabelMap.SfaEstimatedAmount}", text = @TextField),
            @FormField(name = "estimatedProbability", title = "${uiLabelMap.SfaProbability}", position = 2, text = @TextField),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaign}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", keyFieldName = "marketingCampaignId"))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "nextStepDate", title = "${uiLabelMap.SfaNextStepDate}", dateTime = @DateTimeField),
            @FormField(name = "estimatedCloseDate", title = "${uiLabelMap.SfaCloseDate}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "opportunityStageId", title = "${uiLabelMap.SfaInitialStage}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SalesOpportunityStage", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "sequenceNum")}))),
            @FormField(name = "typeEnumId", title = "${uiLabelMap.SfaType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "SLSOPP_TYP_ENUM")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaDataSourceLabel}", useWhen = "communicationEvent==null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataSource", description = "${description}", keyFieldName = "dataSourceId", constraints = {@EntityConstraint(name = "dataSourceTypeId", value = "LEAD_SOURCE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaDataSourceLabel}", useWhen = "communicationEvent!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataSource", description = "${description}", keyFieldName = "dataSourceId", constraints = {@EntityConstraint(name = "dataSourceTypeId", value = "LEAD_SOURCE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "accountPartyId", title = "${uiLabelMap.SfaInitialAccount}", useWhen = "communicationEvent==null", lookup = @LookupField(targetFormName = "LookupAccounts", defaultValue = "${accountPartyId}")),
            @FormField(name = "accountPartyId", title = "${uiLabelMap.SfaInitialAccount}", useWhen = "communicationEvent!=null", lookup = @LookupField(targetFormName = "LookupAccounts", defaultValue = "${accountPartyId}")),
            @FormField(name = "leadPartyId", title = "${uiLabelMap.SfaLead}", useWhen = "communicationEvent==null", lookup = @LookupField(targetFormName = "LookupLeads", defaultValue = "${leadPartyId}")),
            @FormField(name = "leadPartyId", title = "${uiLabelMap.SfaLead}", useWhen = "communicationEvent!=null", lookup = @LookupField(targetFormName = "LookupLeads", defaultValue = "${communicationEvent.partyIdFrom}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", useWhen = "salesOpportunity==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "salesOpportunity!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "salesOpportunity==null", target = "createSalesOpportunity")
        }
    )
    public interface EditSalesOpportunity {}

    @Form(
        name = "ViewSalesOpportunity",
        location = "component://marketing/widget/sfa/forms/OpportunityForms.xml",
        defaultMapName = "salesOpportunity",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "salesOpportunityId", display = @DisplayField),
            @FormField(name = "opportunityName", title = "${uiLabelMap.SfaOpportunityName}", display = @DisplayField),
            @FormField(name = "accountPartyId", mapName = "accountParty", fieldName = "partyId", title = "${uiLabelMap.SfaInitialAccount}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "leadPartyId", mapName = "leadParty", fieldName = "partyId", title = "${uiLabelMap.SfaLead}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "estimatedAmount", title = "${uiLabelMap.SfaEstimatedAmount} ${currencyUomId}", display = @DisplayField),
            @FormField(name = "estimatedProbability", title = "${uiLabelMap.SfaProbability}", position = 2, display = @DisplayField),
            @FormField(name = "nextStepDate", title = "${uiLabelMap.SfaNextStepDate}", display = @DisplayField),
            @FormField(name = "estimatedCloseDate", title = "${uiLabelMap.SfaCloseDate}", position = 2, display = @DisplayField),
            @FormField(name = "opportunityStageId", title = "${uiLabelMap.SfaInitialStage}", displayEntity = @DisplayEntityField(entityName = "SalesOpportunityStage", description = "${description}")),
            @FormField(name = "typeEnumId", title = "${uiLabelMap.SfaType}", position = 2, displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaign}", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, display = @DisplayField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaDataSourceLabel}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.MarketingSegmentGroupDescription}", display = @DisplayField),
            @FormField(name = "nextStep", title = "${uiLabelMap.SfaNextStep}", display = @DisplayField)
        }
    )
    public interface ViewSalesOpportunity {}

}
