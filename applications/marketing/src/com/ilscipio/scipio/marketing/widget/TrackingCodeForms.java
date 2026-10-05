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
public class TrackingCodeForms {

    @Form(
        name = "EditTrackingCode",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "updateTrackingCode",
        defaultMapName = "trackingCode",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", useWhen = "trackingCode==null&&trackingCodeId==null", text = @TextField),
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${trackingCodeId}]", useWhen = "trackingCode==null&&trackingCodeId!=null", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeDescription}", text = @TextField),
            @FormField(name = "comments", title = "${uiLabelMap.MarketingTrackingCodeComments}", textarea = @TextareaField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrackingCodeType", description = "${description}", keyFieldName = "trackingCodeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", keyFieldName = "marketingCampaignId", orderBy = {@EntityOrderBy(fieldName = "campaignName")}))),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.MarketingTrackingCodeDefaultProdCatalogId}", tooltip = "${uiLabelMap.MarketingTrackingCodeNoOverrideIfEmpty}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProdCatalog", description = "${catalogName}", keyFieldName = "prodCatalogId", orderBy = {@EntityOrderBy(fieldName = "catalogName")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "redirectUrl", title = "${uiLabelMap.MarketingTrackingCodeRedirectUrl}", tooltip = "${uiLabelMap.MarketingTrackingCodeNoRedirectIfEmpty}", text = @TextField(size = 40)),
            @FormField(name = "overrideLogo", title = "${uiLabelMap.MarketingTrackingCodeOverrideLogo}", tooltip = "${uiLabelMap.MarketingTrackingCodeNoOverrideIfEmpty}", text = @TextField(size = 60)),
            @FormField(name = "overrideCss", title = "${uiLabelMap.MarketingTrackingCodeOverrideCss}", tooltip = "${uiLabelMap.MarketingTrackingCodeNoOverrideIfEmpty}", text = @TextField(size = 60)),
            @FormField(name = "trackableLifetime", title = "${uiLabelMap.MarketingTrackingCodeTrackableLifetime}", tooltip = "${uiLabelMap.MarketingTrackingCodeInSeconds}", text = @TextField(size = 10)),
            @FormField(name = "billableLifetime", title = "${uiLabelMap.MarketingTrackingCodeBillableLifetime}", tooltip = "${uiLabelMap.MarketingTrackingCodeInSeconds}", text = @TextField(size = 10)),
            @FormField(name = "groupId", title = "${uiLabelMap.MarketingTrackingCodeGroupId}", text = @TextField(size = 10)),
            @FormField(name = "subgroupId", title = "${uiLabelMap.MarketingTrackingCodeSubgroupId}", text = @TextField(size = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "trackingCode==null", target = "createTrackingCode")
        }
    )
    public interface EditTrackingCode {}

    @Form(
        name = "ListTrackingCode",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        target = "ListTrackingCode",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditTrackingCode", description = "${trackingCodeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trackingCodeId")})),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeDescription}", display = @DisplayField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", displayEntity = @DisplayEntityField(entityName = "TrackingCodeType")),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}")),
            @FormField(name = "prodCatalogId", displayEntity = @DisplayEntityField(entityName = "ProdCatalog", description = "${catalogName}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTrackingCode", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trackingCodeId")}))
        }
    )
    public interface ListTrackingCode {}

    @Form(
        name = "EditTrackingCodeOrder",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "updateTrackingCodeOrder",
        defaultMapName = "trackingCodeOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", display = @DisplayField),
            @FormField(name = "orderId", title = "${uiLabelMap.MarketingTrackingCodeOrderOrderId}", display = @DisplayField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrackingCodeType", description = "${description}", keyFieldName = "trackingCodeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "isBillable", title = "${uiLabelMap.MarketingTrackingCodeOrderIsBilliable}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "trackingCodeOrder==null", target = "createTrackingCodeOrder")
        }
    )
    public interface EditTrackingCodeOrder {}

    @Form(
        name = "FindTrackingCodeOrders",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "ListTrackingCodeOrders",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", textFind = @TextFindField),
            @FormField(name = "orderId", title = "${uiLabelMap.MarketingTrackingCodeOrderOrderId}", lookup = @LookupField(targetFormName = "LookupOrderName")),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrackingCodeType", description = "${description}", keyFieldName = "trackingCodeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindTrackingCodeOrders {}

    @Form(
        name = "ListTrackingCodeOrders",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindTrackingCodeOrders",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", display = @DisplayField),
            @FormField(name = "orderId", title = "${uiLabelMap.MarketingTrackingCodeOrderOrderId}", displayEntity = @DisplayEntityField(entityName = "OrderHeader", description = "${orderDate} [${orderId}]")),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", displayEntity = @DisplayEntityField(entityName = "TrackingCodeType", description = "${description}")),
            @FormField(name = "isBillable", title = "${uiLabelMap.MarketingTrackingCodeOrderIsBilliable}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "-orderId"), @FieldMap(fieldName = "entityName", value = "TrackingCodeOrder"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTrackingCodeOrders {}

    @Form(
        name = "EditTrackingCodeVisit",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "updateTrackingCodeVisit",
        defaultMapName = "visit",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "tackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", display = @DisplayField),
            @FormField(name = "visitId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "sourceEnumId", title = "${uiLabelMap.MarketingTrackingCodeVisitSourceEnumId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = " TRACKINGCODE_SRC")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = "${uiLabelMap.CommonCancel}", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "visit==null", target = "createTrackingCodeVisit")
        }
    )
    public interface EditTrackingCodeVisit {}

    @Form(
        name = "FindTrackingCodeVisits",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "ListTrackingCodeVisits",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", text = @TextField),
            @FormField(name = "visitId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitId}", lookup = @LookupField(targetFormName = "LookupVisit")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "sourceEnumId", title = "${uiLabelMap.MarketingTrackingCodeVisitSourceEnumId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = " TRACKINGCODE_SRC")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindTrackingCodeVisits {}

    @Form(
        name = "ListTrackingCodeVisits",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindTrackingCodeVisits",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "visitId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/visitdetail", urlMode = UrlMode.INTER_APP, description = "${visitId}", parameters = {@ParameterDef(paramName = "visitId"), @ParameterDef(paramName = "DONE_PAGE", fromField = "donePage")})),
            @FormField(name = "sourceEnumId", title = "${uiLabelMap.MarketingTrackingCodeVisitSourceEnumId}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description} [${enumCode}]")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", value = "-fromDate"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTrackingCodeVisits {}

    @Form(
        name = "LookupTrackingCode",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "LookupTrackingCode",
        defaultMapName = "trackingCode",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeDescription}", textFind = @TextFindField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrackingCodeType", description = "${description}", keyFieldName = "trackingCodeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.MarketingTrackingCodeProdCatalogId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdCatalog", description = "${catalogName}", keyFieldName = "prodCatalogId", orderBy = {@EntityOrderBy(fieldName = "catalogName")}))),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", orderBy = {@EntityOrderBy(fieldName = "campaignName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface LookupTrackingCode {}

    @Form(
        name = "ListLookupTrackingCode",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${trackingCodeId}')", urlMode = UrlMode.PLAIN, description = "${trackingCodeId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeDescription}", display = @DisplayField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", displayEntity = @DisplayEntityField(entityName = "TrackingCodeType")),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.MarketingTrackingCodeProdCatalogId}", displayEntity = @DisplayEntityField(entityName = "ProdCatalog", description = "${catalogName}")),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupTrackingCode {}

    @Form(
        name = "LookupVisit",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "LookupVisit",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "visitId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitId}", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingTrackingCodeVisitPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateFind = @DateFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupVisit {}

    @Form(
        name = "ListLookupVisit",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "visitId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${visitId}')", urlMode = UrlMode.PLAIN, description = "${visitId}", alsoHidden = false)),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingTrackingCodeVisitPartyId}", displayEntity = @DisplayEntityField(entityName = "Party", keyFieldName = "partyId", description = "${partyId}")),
            @FormField(name = "visitorId", title = "${uiLabelMap.MarketingTrackingCodeVisitVisitorId}", displayEntity = @DisplayEntityField(entityName = "Visitor", keyFieldName = "visitorId", description = "${partyId} ${userLoginId} [${visitorId}]")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingTrackingCodeVisitContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.MarketingTrackingCodeVisitRoleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "userLoginId", title = "${uiLabelMap.MarketingTrackingCodeVisitUserLoginId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "clientIpAddress", title = "${uiLabelMap.MarketingTrackingCodeVisitClientIpAddress}", display = @DisplayField),
            @FormField(name = "clientHostName", title = "${uiLabelMap.MarketingTrackingCodeVisitClientHostName}", display = @DisplayField),
            @FormField(name = "webappName", title = "${uiLabelMap.MarketingTrackingCodeVisitWebappName}", display = @DisplayField),
            @FormField(name = "sessionId", title = "${uiLabelMap.MarketingTrackingCodeVisitSessionId}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupVisit {}

    @Form(
        name = "EditTrackingCodeType",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "updateTrackingCodeType",
        defaultMapName = "trackingCodeType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "trackingCodeType!=null", display = @DisplayField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", useWhen = "trackingCodeType==null&&trackingCodeTypeId==null", text = @TextField),
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${trackingCodeTypeId}]", useWhen = "trackingCodeType==null&&trackingCodeTypeId!=null", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeTypeDescription}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "trackingCodeType==null", target = "createTrackingCodeType")
        }
    )
    public interface EditTrackingCodeType {}

    @Form(
        name = "ListTrackingCodeType",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        target = "FindTrackingCodeType",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditTrackingCodeType", description = "${trackingCodeTypeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trackingCodeTypeId")})),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeTypeDescription}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTrackingCodeType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trackingCodeTypeId")}))
        }
    )
    public interface ListTrackingCodeType {}

    @Form(
        name = "LookupTrackingCodeType",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        target = "LookupTrackingCodeType",
        defaultMapName = "trackingCode",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeTypeDescription}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface LookupTrackingCodeType {}

    @Form(
        name = "ListLookupTrackingCodeType",
        location = "component://marketing/widget/TrackingCodeForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trackingCodeTypeId", title = "${uiLabelMap.MarketingTrackingCodeTrackingCodeTypeId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${trackingCodeTypeId}')", urlMode = UrlMode.PLAIN, description = "${trackingCodeTypeId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.MarketingTrackingCodeTypeDescription}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupTrackingCodeType {}

}
