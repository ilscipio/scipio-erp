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
package com.ilscipio.scipio.product.widget;

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
public class FacilityShipmentGatewayConfigForms {

    @Form(
        name = "FindShipmentGatewayConfig",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "FindShipmentGatewayConfig",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentGatewayConfigId", title = "${uiLabelMap.FacilityShipmentGatewayConfigId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigDescription}", textFind = @TextFindField),
            @FormField(name = "shipmentGatewayConfTypeId", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentGatewayConfigType", description = "${description}", keyFieldName = "shipmentGatewayConfTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindShipmentGatewayConfig {}

    @Form(
        name = "ListShipmentGatewayConfig",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayConfig", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigDescription}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditShipmentGatewayConfig?shipmentGatewayConfigId=${shipmentGatewayConfigId}", description = "${description}")),
            @FormField(name = "shipmentGatewayConfTypeId", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeId}", displayEntity = @DisplayEntityField(entityName = "ShipmentGatewayConfigType", keyFieldName = "shipmentGatewayConfTypeId", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ShipmentGatewayConfig"), @FieldMap(fieldName = "orderBy", value = "description"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListShipmentGatewayConfig {}

    @Form(
        name = "EditShipmentGatewayConfig",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfig",
        defaultMapName = "shipmentGatewayConfig",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigDescription}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "shipmentGatewayConfTypeId", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeId}", displayEntity = @DisplayEntityField(entityName = "ShipmentGatewayConfigType", keyFieldName = "shipmentGatewayConfTypeId", description = "${description}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfig {}

    @Form(
        name = "EditShipmentGatewayConfigDhl",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfigDhl",
        defaultMapName = "shipmentGatewayDhl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayDhl", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "connectUrl", title = "${uiLabelMap.FacilityShipmentDhlConnectUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "connectTimeout", title = "${uiLabelMap.FacilityShipmentDhlConnectTimeout}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "headVersion", title = "${uiLabelMap.FacilityShipmentDhlHeadVersion}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "headAction", title = "${uiLabelMap.FacilityShipmentDhlHeadAction}", dropDown = @DropDownField(options = {@Option(key = "Request", description = "${uiLabelMap.FacilityShipmentDhlHeadActionRequest}")})),
            @FormField(name = "accessUserId", title = "${uiLabelMap.FacilityShipmentDhlAccessUserId}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "accessPassword", title = "${uiLabelMap.FacilityShipmentDhlAccessPassword}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "accessAccountNbr", title = "${uiLabelMap.FacilityShipmentDhlAccessAccountNbr}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "accessShippingKey", title = "${uiLabelMap.FacilityShipmentDhlAccessShippingKey}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "labelImageFormat", title = "${uiLabelMap.FacilityShipmentDhlLabelImageFormat}", dropDown = @DropDownField(options = {@Option(key = "PNG", description = "${uiLabelMap.FacilityShipmentDhlLabelImageFormatPng}")})),
            @FormField(name = "rateEstimateTemplate", title = "${uiLabelMap.FacilityShipmentDhlRateEstimate}", dropDown = @DropDownField(options = {@Option(key = "api.schema.DHL", description = "${uiLabelMap.FacilityShipmentDhlRateEstimate}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfigDhl {}

    @Form(
        name = "EditShipmentGatewayConfigFedex",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfigFedex",
        defaultMapName = "shipmentGatewayFedex",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayFedex", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "connectUrl", title = "${uiLabelMap.FacilityShipmentFedexConnectUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "connectSoapUrl", title = "${uiLabelMap.FacilityShipmentFedexConnectSoapUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "connectTimeout", title = "${uiLabelMap.FacilityShipmentFedexConnectTimeout}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "accessAccountNbr", title = "${uiLabelMap.FacilityShipmentFedexAccessAccountNumber}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessMeterNumber", title = "${uiLabelMap.FacilityShipmentFedexAccessMeterNumber}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessUserKey", title = "${uiLabelMap.FacilityShipmentFedexAccessUserKey}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessUserPwd", title = "${uiLabelMap.FacilityShipmentFedexAccessUserPwd}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "labelImageType", title = "${uiLabelMap.FacilityShipmentFedexLabelImageType}", dropDown = @DropDownField(options = {@Option(key = "PDF", description = "${uiLabelMap.FacilityShipmentFedexLabelImageTypePdf}"), @Option(key = "PNG", description = "${uiLabelMap.FacilityShipmentFedexLabelImageTypePng}")})),
            @FormField(name = "defaultDropoffType", title = "${uiLabelMap.FacilityShipmentFedexDropoffType}", dropDown = @DropDownField(options = {@Option(key = "REGULARPICKUP", description = "${uiLabelMap.FacilityShipmentFedexDropoffTypeRegularPickup}"), @Option(key = "REQUESTCOURIER", description = "${uiLabelMap.FacilityShipmentFedexDropoffTypeRequestCourier}"), @Option(key = "DROPBOX", description = "${uiLabelMap.FacilityShipmentFedexDropoffTypeDropBox}"), @Option(key = "BUSINESSSERVICECTR", description = "${uiLabelMap.FacilityShipmentFedexDropoffTypeBusinessService}"), @Option(key = "STATION", description = "${uiLabelMap.FacilityShipmentFedexDropoffTypeStation}")})),
            @FormField(name = "defaultPackagingType", title = "${uiLabelMap.FacilityShipmentFedexPackingType}", dropDown = @DropDownField(options = {@Option(key = "FXENV", description = "${uiLabelMap.FacilityShipmentFedexPackingEnveloper}"), @Option(key = "FXENV_LGL", description = "${uiLabelMap.FacilityShipmentFedexPackingEnveloperLegal}"), @Option(key = "FXPAK_SM", description = "${uiLabelMap.FacilityShipmentFedexPackingPakSmall}"), @Option(key = "FXPAK_LRG", description = "${uiLabelMap.FacilityShipmentFedexPackingPakLarge}"), @Option(key = "FXBOX_SM", description = "${uiLabelMap.FacilityShipmentFedexPackingBoxSmall}"), @Option(key = "FXBOX_MED", description = "${uiLabelMap.FacilityShipmentFedexPackingBoxMedium}"), @Option(key = "FXBOX_LRG", description = "${uiLabelMap.FacilityShipmentFedexPackingBoxLarge}"), @Option(key = "FXTUBE", description = "${uiLabelMap.FacilityShipmentFedexPackingTube}"), @Option(key = "FX10KGBOX", description = "${uiLabelMap.FacilityShipmentFedexPackingBox10Kg}"), @Option(key = "FX25KGBOX", description = "${uiLabelMap.FacilityShipmentFedexPackingBox25Kg}"), @Option(key = "YOURPACKNG", description = "${uiLabelMap.FacilityShipmentFedexPackingYour}")})),
            @FormField(name = "templateShipment", title = "${uiLabelMap.FacilityShipmentFedexShipmentTemplateLocation}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "templateSubscription", title = "${uiLabelMap.FacilityShipmentFedexSubscriptionTemplateLocation}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "rateEstimateTemplate", title = "${uiLabelMap.FacilityShipmentFedexRateEstimateTemplateLocation}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfigFedex {}

    @Form(
        name = "EditShipmentGatewayConfigUps",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfigUps",
        defaultMapName = "shipmentGatewayUps",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayUps", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "connectUrl", title = "${uiLabelMap.FacilityShipmentUpsConnectUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "connectTimeout", title = "${uiLabelMap.FacilityShipmentUpsConnectTimeout}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "shipperNumber", title = "${uiLabelMap.FacilityShipmentUpsShipperNumber}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "billShipperAccountNumber", title = "${uiLabelMap.FacilityShipmentUpsBillShipperAccountNumber}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessLicenseNumber", title = "${uiLabelMap.FacilityShipmentUpsAccessLicenseNumber}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessUserId", title = "${uiLabelMap.FacilityShipmentUpsAccessUserId}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessPassword", title = "${uiLabelMap.FacilityShipmentUpsAccessPassword}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "saveCertInfo", title = "${uiLabelMap.FacilityShipmentUpsSaveCertInfo}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "saveCertPath", title = "${uiLabelMap.FacilityShipmentUpsSaveCertPath}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "shipperPickupType", title = "${uiLabelMap.FacilityShipmentUpsShipperPickupType}", dropDown = @DropDownField(options = {@Option(key = "01", description = "${uiLabelMap.FacilityShipmentUpsPickupDaily}"), @Option(key = "03", description = "${uiLabelMap.FacilityShipmentUpsPickupCustomerCounter}"), @Option(key = "06", description = "${uiLabelMap.FacilityShipmentUpsPickupOneTime}"), @Option(key = "07", description = "${uiLabelMap.FacilityShipmentUpsPickupOnCallAir}"), @Option(key = "11", description = "${uiLabelMap.FacilityShipmentUpsPickupSuggestedRetailRates}"), @Option(key = "19", description = "${uiLabelMap.FacilityShipmentUpsPickupLetterCenter}"), @Option(key = "20", description = "${uiLabelMap.FacilityShipmentUpsPickupAirServiceCenter}")})),
            @FormField(name = "customerClassification", title = "${uiLabelMap.FacilityShipmentUpsCustomerClassification}", dropDown = @DropDownField(options = {@Option(key = "01", description = "${uiLabelMap.FacilityShipmentUpsCustomerClassificationWholesale}"), @Option(key = "03", description = "${uiLabelMap.FacilityShipmentUpsCustomerClassificationOccasional}"), @Option(key = "04", description = "${uiLabelMap.FacilityShipmentUpsCustomerClassificationRetail}")})),
            @FormField(name = "maxEstimateWeight", title = "${uiLabelMap.FacilityShipmentUpsMaxEstimateWeight}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "minEstimateWeight", title = "${uiLabelMap.FacilityShipmentUpsMinEstimateWeight}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "codAllowCod", title = "${uiLabelMap.FacilityShipmentUpsAllowCod}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "codSurchargeAmount", title = "${uiLabelMap.FacilityShipmentUpsSurchargeAmount}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "codSurchargeCurrencyUomId", title = "${uiLabelMap.FacilityShipmentUpsSurchargeCurrencyUomId}", text = @TextField(size = 3, maxlength = 3)),
            @FormField(name = "codSurchargeApplyToPackage", title = "${uiLabelMap.FacilityShipmentUpsSurchargeApplyToPackage}", dropDown = @DropDownField(options = {@Option(key = "all", description = "${uiLabelMap.FacilityShipmentUpsSurchargeAll}"), @Option(key = "first", description = "${uiLabelMap.FacilityShipmentUpsSurchargeFirst}"), @Option(key = "split", description = "${uiLabelMap.FacilityShipmentUpsSurchargeSplit}"), @Option(key = "none", description = "${uiLabelMap.FacilityShipmentUpsSurchargeNone}")})),
            @FormField(name = "codFundsCode", title = "${uiLabelMap.FacilityShipmentUpsFundsCode}", dropDown = @DropDownField(options = {@Option(key = "0", description = "${uiLabelMap.FacilityShipmentUpsUnsecuredFundsAllowed}"), @Option(key = "8", description = "${uiLabelMap.FacilityShipmentUpsSecuredFundsOnly}")})),
            @FormField(name = "defaultReturnLabelMemo", title = "${uiLabelMap.FacilityShipmentUpsDefaultReturnLabelMemo}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "defaultReturnLabelSubject", title = "${uiLabelMap.FacilityShipmentUpsDefaultReturnLabelSubject}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfigUps {}

    @Form(
        name = "EditShipmentGatewayConfigUsps",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfigUsps",
        defaultMapName = "shipmentGatewayUsps",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayUsps", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "connectUrl", title = "${uiLabelMap.FacilityShipmentUspsConnectUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "connectTimeout", title = "${uiLabelMap.FacilityShipmentUspsConnectTimeout}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "accessUserId", title = "${uiLabelMap.FacilityShipmentUspsAccessUserId}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "accessPassword", title = "${uiLabelMap.FacilityShipmentUspsAccessPassword}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "maxEstimateWeight", title = "${uiLabelMap.FacilityShipmentUspsMaxEstimateWeight}", text = @TextField(size = 9, maxlength = 9)),
            @FormField(name = "test", title = "${uiLabelMap.FacilityShipmentUspsTestMode}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfigUsps {}

    @Form(
        name = "FindShipmentGatewayConfigTypes",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "FindShipmentGatewayConfigTypes",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentGatewayConfTypeId", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigDescription}", textFind = @TextFindField),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindShipmentGatewayConfigTypes {}

    @Form(
        name = "ListShipmentGatewayConfigTypes",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentGatewayConfigType", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "shipmentGatewayConfTypeId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeDescription}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditShipmentGatewayConfigType?shipmentGatewayConfTypeId=${shipmentGatewayConfTypeId}", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ShipmentGatewayConfigType"), @FieldMap(fieldName = "orderBy", value = "description DESC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListShipmentGatewayConfigTypes {}

    @Form(
        name = "EditShipmentGatewayConfigType",
        location = "component://product/widget/facility/ShipmentGatewayConfigForms.xml",
        target = "UpdateShipmentGatewayConfigType",
        defaultMapName = "shipmentGatewayConfigType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentGatewayConfTypeId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypeDescription}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "parentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentGatewayConfigType", description = "${description}", keyFieldName = "shipmentGatewayConfTypeId", constraints = {@EntityConstraint(name = "shipmentGatewayConfTypeId", envName = "shipmentGatewayConfTypeId", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "hasTable", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShipmentGatewayConfigType {}

}
