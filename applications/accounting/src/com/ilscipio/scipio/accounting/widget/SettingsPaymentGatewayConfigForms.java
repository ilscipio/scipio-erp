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
package com.ilscipio.scipio.accounting.widget;

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
public class SettingsPaymentGatewayConfigForms {

    @Form(
        name = "FindPaymentGatewayConfig",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "FindPaymentGatewayConfig",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentGatewayConfigId", title = "${uiLabelMap.AccountingPaymentGatewayConfigId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.AccountingPaymentGatewayConfigDescription}", textFind = @TextFindField),
            @FormField(name = "paymentGatewayConfigTypeId", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentGatewayConfigType", description = "${description}", keyFieldName = "paymentGatewayConfigTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPaymentGatewayConfig {}

    @Form(
        name = "ListPaymentGatewayConfig",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayConfig", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "description", title = "${uiLabelMap.AccountingPaymentGatewayConfigDescription}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditPaymentGatewayConfig?paymentGatewayConfigId=${paymentGatewayConfigId}", description = "${description}")),
            @FormField(name = "paymentGatewayConfigTypeId", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeId}", displayEntity = @DisplayEntityField(entityName = "PaymentGatewayConfigType", keyFieldName = "paymentGatewayConfigTypeId", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PaymentGatewayConfig"), @FieldMap(fieldName = "orderBy", value = "description"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPaymentGatewayConfig {}

    @Form(
        name = "EditPaymentGatewayConfig",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfig",
        defaultMapName = "paymentGatewayConfig",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentGatewayConfigId", hidden = @HiddenField),
            @FormField(name = "description", mapName = "paymentGatewayConfig", title = "${uiLabelMap.AccountingPaymentGatewayConfigDescription}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "paymentGatewayConfigTypeId", mapName = "paymentGatewayConfig", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeId}", displayEntity = @DisplayEntityField(entityName = "PaymentGatewayConfigType", keyFieldName = "paymentGatewayConfigTypeId", description = "${description}"))
        }
    )
    public interface EditPaymentGatewayConfig {}

    @Form(
        name = "EditPaymentGatewayConfigSagePay",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigSagePay",
        defaultMapName = "paymentGatewaySagePay",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewaySagePay", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "vendor", title = "${uiLabelMap.AccountingSagePayVendor}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "productionHost", title = "${uiLabelMap.AccountingSagePayProductionHost}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "testingHost", title = "${uiLabelMap.AccountingSagePayTestingHost}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "sagePayMode", title = "${uiLabelMap.AccountingSagePayMode}", dropDown = @DropDownField(options = {@Option(key = "TEST", description = "${uiLabelMap.AccountingSagePayTest}"), @Option(key = "PRODUCTION", description = "${uiLabelMap.AccountingSagePayProduction}")})),
            @FormField(name = "protocolVersion", title = "${uiLabelMap.AccountingSagePayProtocolVersion}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "authenticationTransType", title = "${uiLabelMap.AccountingSagePayAuthenticationTransType}", dropDown = @DropDownField(options = {@Option(key = "PAYMENT", description = "${uiLabelMap.CommonPayment}"), @Option(key = "AUTHENTICATE", description = "${uiLabelMap.CommonAuthenticate}"), @Option(key = "DEFERRED", description = "${uiLabelMap.CommonDeferred}")})),
            @FormField(name = "authenticationUrl", title = "${uiLabelMap.AccountingSagePayAuthenticationUrl}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "authoriseTransType", title = "${uiLabelMap.AccountingSagePayAuthorisationTransType}", dropDown = @DropDownField(options = {@Option(key = "AUTHORISE", description = "${uiLabelMap.CommonAuthorise}"), @Option(key = "RELEASE", description = "${uiLabelMap.CommonRelease}")})),
            @FormField(name = "authoriseUrl", title = "${uiLabelMap.AccountingSagePayAuthorisationUrl}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "releaseTransType", title = "${uiLabelMap.AccountingSagePayReleaseTransType}", dropDown = @DropDownField(options = {@Option(key = "CANCEL", description = "${uiLabelMap.CommonCancel}"), @Option(key = "ABORT", description = "${uiLabelMap.CommonAbort}")})),
            @FormField(name = "releaseUrl", title = "${uiLabelMap.AccountingSagePayReleaseUrl}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "voidUrl", title = "${uiLabelMap.AccountingSagePayVoidUrl}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "refundUrl", title = "${uiLabelMap.AccountingSagePayRefundUrl}", text = @TextField(size = 100, maxlength = 100)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigSagePay {}

    @Form(
        name = "EditPaymentGatewayConfigAuthorizeNet",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigAuthorizeNet",
        defaultMapName = "paymentGatewayAuthorizeNet",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayAuthorizeNet", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "transactionUrl", title = "${uiLabelMap.AccountingAuthorizeNetTransactionUrl}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "apiVersion", title = "${uiLabelMap.AccountingAuthorizeNetApiVersion}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "delimitedData", title = "${uiLabelMap.AccountingAuthorizeNetDelimitedData}", dropDown = @DropDownField(options = {@Option(key = "TRUE", description = "${uiLabelMap.CommonTrue}"), @Option(key = "FALSE", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "delimiterChar", title = "${uiLabelMap.AccountingAuthorizeNetDelimiterChar}", text = @TextField(size = 1, maxlength = 1)),
            @FormField(name = "method", title = "${uiLabelMap.AccountingAuthorizeNetMethod}", dropDown = @DropDownField(options = {@Option(key = "CC", description = "${uiLabelMap.AccountingAuthorizeNetMethodCC}")})),
            @FormField(name = "emailCustomer", title = "${uiLabelMap.AccountingAuthorizeNetEmailCustomer}", dropDown = @DropDownField(options = {@Option(key = "TRUE", description = "${uiLabelMap.CommonTrue}"), @Option(key = "FALSE", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "emailMerchant", title = "${uiLabelMap.AccountingAuthorizeNetEmailMerchant}", dropDown = @DropDownField(options = {@Option(key = "TRUE", description = "${uiLabelMap.CommonTrue}"), @Option(key = "FALSE", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "testMode", title = "${uiLabelMap.AccountingAuthorizeNetTestMode}", dropDown = @DropDownField(options = {@Option(key = "TRUE", description = "${uiLabelMap.CommonTrue}"), @Option(key = "FALSE", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "relayResponse", title = "${uiLabelMap.AccountingAuthorizeNetRelayResponse}", dropDown = @DropDownField(options = {@Option(key = "TRUE", description = "${uiLabelMap.CommonTrue}"), @Option(key = "FALSE", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "cpVersion", title = "${uiLabelMap.AccountingAuthorizeNetCpVersion}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "cpMarketType", title = "${uiLabelMap.AccountingAuthorizeNetCpMarket}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "cpDeviceType", title = "${uiLabelMap.AccountingAuthorizeNetCpDevice}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "tranKey", title = "${uiLabelMap.AccountingAuthorizeNetTransKey}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigAuthorizeNet {}

    @Form(
        name = "EditPaymentGatewayConfigCyberSource",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigCyberSource",
        defaultMapName = "paymentGatewayCyberSource",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayCyberSource", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "apiVersion", title = "${uiLabelMap.AccountingCyberSourceApiVersion}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "production", title = "${uiLabelMap.AccountingCyberSourceProduction}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "keysDir", title = "${uiLabelMap.AccountingCyberSourceKeysDir}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "keysFile", title = "${uiLabelMap.AccountingCyberSourceKeysFile}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "logEnabled", title = "${uiLabelMap.AccountingCyberSourceLogEnable}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "logDir", title = "${uiLabelMap.AccountingCyberSourceLogDir}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "logFile", title = "${uiLabelMap.AccountingCyberSourceLogFile}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "logSize", title = "${uiLabelMap.AccountingCyberSourceLogSize}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "merchantDescr", title = "${uiLabelMap.AccountingCyberSourceMerchantDescr}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "merchantContact", title = "${uiLabelMap.AccountingCyberSourceMerchantContact}", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "autoBill", title = "${uiLabelMap.AccountingCyberSourceAutoBill}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "ignoreAvs", title = "${uiLabelMap.AccountingCyberSourceIgnoreAvs}", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "enableDav", title = "${uiLabelMap.AccountingCyberSourceEnableDav}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "fraudScore", title = "${uiLabelMap.AccountingCyberSourceFraudScore}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "disableBillAvs", title = "${uiLabelMap.AccountingCyberSourceDisableBillAvs}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "avsDeclineCodes", title = "${uiLabelMap.AccountingCyberSourceAvsDeclineCodes}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigCyberSource {}

    @Form(
        name = "EditPaymentGatewayConfigPayflowPro",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigPayflowPro",
        defaultMapName = "paymentGatewayPayflowPro",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayPayflowPro", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "certsPath", text = @TextField(size = 80, maxlength = 80)),
            @FormField(name = "checkAvs", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "checkCvv2", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "preAuth", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "enableTransmit", dropDown = @DropDownField(options = {@Option(key = "true", description = "${uiLabelMap.CommonTrue}"), @Option(key = "false", description = "${uiLabelMap.CommonFalse}")})),
            @FormField(name = "loggingLevel", dropDown = @DropDownField(options = {@Option(key = "6", description = "${uiLabelMap.AccountingPayflowProLoggingOff}"), @Option(key = "5", description = "${uiLabelMap.AccountingPayflowProLoggingSeverityFatal}"), @Option(key = "4", description = "${uiLabelMap.AccountingPayflowProLoggingSeverityError}"), @Option(key = "3", description = "${uiLabelMap.AccountingPayflowProLoggingSeverityWarn}"), @Option(key = "2", description = "${uiLabelMap.AccountingPayflowProLoggingSeverityInfo}"), @Option(key = "1", description = "${uiLabelMap.AccountingPayflowProLoggingSeverityDebug}")})),
            @FormField(name = "maxLogFileSize", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "stackTraceOn", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigPayflowPro {}

    @Form(
        name = "EditPaymentGatewayConfigClearCommerce",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigClearCommerce",
        defaultMapName = "paymentGatewayClearCommerce",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayClearCommerce", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "enableCVM", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "processMode", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.AccountingPaymentGatewayApprove}"), @Option(key = "N", description = "${uiLabelMap.AccountingPaymentGatewayDecline}"), @Option(key = "R", description = "${uiLabelMap.AccountingPaymentGatewayRandom}"), @Option(key = "P", description = "${uiLabelMap.AccountingPaymentGatewayProduction}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigClearCommerce {}

    @Form(
        name = "EditPaymentGatewayConfigEway",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigEway",
        defaultMapName = "paymentGatewayEway",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayEway", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "testMode", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "enableBeagle", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "enableCvn", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigEway {}

    @Form(
        name = "EditPaymentGatewayConfigWorldPay",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigWorldPay",
        defaultMapName = "paymentGatewayWorldPay",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayWorldPay", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "redirectUrl", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "instId", title = "${uiLabelMap.AccountingWorldPayInstId}", text = @TextField(size = 10, maxlength = 10)),
            @FormField(name = "authMode", title = "${uiLabelMap.AccountingWorldPayAuthMode}", dropDown = @DropDownField(options = {@Option(key = "A", description = "${uiLabelMap.AccountingWorldPayFullAuth}"), @Option(key = "E", description = "${uiLabelMap.AccountingWorldPayPreAuth}")})),
            @FormField(name = "fixContact", title = "${uiLabelMap.AccountingWorldPayFixContact}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "hideContact", title = "${uiLabelMap.AccountingWorldPayHideContact}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "hideCurrency", title = "${uiLabelMap.AccountingWorldPayHideCurrency}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "langId", title = "${uiLabelMap.AccountingWorldPayLangId}", text = @TextField(size = 6, maxlength = 6)),
            @FormField(name = "noLanguageMenu", title = "${uiLabelMap.AccountingWorldPayNoLanguageMenu}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "withDelivery", title = "${uiLabelMap.AccountingWorldPayWithDelivery}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "testMode", title = "${uiLabelMap.AccountingWorldPayTestMode}", dropDown = @DropDownField(options = {@Option(key = "100", description = "${uiLabelMap.AccountingWorldPayApprove}"), @Option(key = "101", description = "${uiLabelMap.AccountingWorldPayCancelled}"), @Option(key = "0", description = "${uiLabelMap.AccountingWorldPayLive}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigWorldPay {}

    @Form(
        name = "FindPaymentGatewayConfigTypes",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "FindPaymentGatewayConfigTypes",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentGatewayConfigTypeId", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.AccountingPaymentGatewayConfigDescription}", textFind = @TextFindField),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPaymentGatewayConfigTypes {}

    @Form(
        name = "ListPaymentGatewayConfigTypes",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayConfigType", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "paymentGatewayConfigTypeId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeDescription}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditPaymentGatewayConfigType?paymentGatewayConfigTypeId=${paymentGatewayConfigTypeId}", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PaymentGatewayConfigType"), @FieldMap(fieldName = "orderBy", value = "description DESC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPaymentGatewayConfigTypes {}

    @Form(
        name = "EditPaymentGatewayConfigType",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigType",
        defaultMapName = "paymentGatewayConfigType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentGatewayConfigTypeId", hidden = @HiddenField),
            @FormField(name = "description", title = "${uiLabelMap.AccountingPaymentGatewayConfigTypeDescription}", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "parentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentGatewayConfigType", description = "${description}", keyFieldName = "paymentGatewayConfigTypeId", constraints = {@EntityConstraint(name = "paymentGatewayConfigTypeId", envName = "paymentGatewayConfigTypeId", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "hasTable", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigType {}

    @Form(
        name = "EditPaymentGatewayConfigPayPal",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigPayPal",
        defaultMapName = "paymentGatewayPayPal",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayPayPal", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigPayPal {}

    @Form(
        name = "EditPaymentGatewayConfigSecurePay",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigSecurePay",
        defaultMapName = "paymentGatewaySecurePay",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewaySecurePay", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigSecurePay {}

    @Form(
        name = "EditPaymentGatewayConfigiDEAL",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigiDEAL",
        defaultMapName = "paymentGatewayiDEAL",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayiDEAL", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigiDEAL {}

    @Form(
        name = "EditPaymentGatewayConfigOrbital",
        location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml",
        target = "UpdatePaymentGatewayConfigOrbital",
        defaultMapName = "paymentGatewayOrbital",
        extendsForm = "EditPaymentGatewayConfig",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayOrbital", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentGatewayConfigOrbital {}

}
