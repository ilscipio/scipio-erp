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
public class LookupScreens {

    @Screen(name = "LookupPayment", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupPayment}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Payment")
    @Action(type = ActionType.SET, field = "searchFields", value = "[paymentId, partyIdFrom, partyIdTo, currencyUomId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPayment", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listPayment", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupPayment {}

    @Screen(name = "LookupInvoice", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupInvoices}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Invoice")
    @Action(type = ActionType.SET, field = "searchFields", value = "[invoiceId, partyIdFrom, partyId, description, currencyUomId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupInvoice", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listInvoice", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupInvoice {}

    @Screen(name = "LookupFixedAsset", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupFixedAsset}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "FixedAsset")
    @Action(type = ActionType.SET, field = "searchFields", value = "[fixedAssetId, fixedAssetName]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupFixedAsset", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupFixedAsset", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupFixedAsset {}

    @Screen(name = "LookupBillingAccount", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupBillingAccount}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "BillingAccount")
    @Action(type = ActionType.SET, field = "searchFields", value = "[billingAccountId, description, externalAccountId, accountCurrencyUomId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupBillingAccount", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listBillingAccount", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupBillingAccount {}

    @Screen(name = "LookupGlAccount", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupGlAccount}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "GlAccount")
    @Action(type = ActionType.SET, field = "searchFields", value = "[glAccountId, accountName, glAccountTypeId, glAccountClassId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupGlAccount", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupGlAccount", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupGlAccount {}

    @Screen(name = "LookupAgreement", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupAgreement}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Agreement")
    @Action(type = ActionType.SET, field = "searchFields", value = "[agreementId, productId, description]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupAgreement", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listAgreements", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupAgreement {}

    @Screen(name = "LookupAgreementItem", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupAgreementItem}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "AgreementItem")
    @Action(type = ActionType.SET, field = "searchFields", value = "[agreementItemSeqId, agreementId, currencyUomId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupAgreementItem", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listAgreementItems", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupAgreementItem {}

    @Screen(name = "LookupPaymentGroupMember", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupPaymentGroupMember}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "PaymentGroupMember")
    @Action(type = ActionType.SET, field = "searchFields", value = "[paymentGroupId, paymentId, sequenceNum]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPaymentGroupMember", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listPaymentGroupMember", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupPaymentGroupMember {}

    @Screen(name = "LookupGlReconciliation", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupGlReconciliations}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "GlReconciliation")
    @Action(type = ActionType.SET, field = "searchFields", value = "[glReconciliationId, glReconciliationName, description]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupGlReconciliation", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupReconciliation", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupGlReconciliation {}

    @Screen(name = "LookupCustomTimePeriod", location = "component://accounting/widget/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupCustomTimePeriod}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "CustomTimePeriod")
    @Action(type = ActionType.SET, field = "searchFields", value = "[customTimePeriodId, periodName]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupCustomTimePeriod", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupCustomTimePeriod", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupCustomTimePeriod {}

    @Screen(name = "LookupTaxAuthorityGeo", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupGeo}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "TaxAuthorityAndGeo")
    @Action(type = ActionType.SET, field = "searchFields", value = "[geoId, geoName, geoCode]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[geoId, geoName, geoCode]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupGeo", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupGeo", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupTaxAuthorityGeo {}

    @Screen(name = "LookupTaxAuthorityPartyName", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyLookupPartyByName}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "TaxAuthorityAndPartyNameView")
    @Action(type = ActionType.SET, field = "searchFields", value = "[partyId, firstName, middleName, lastName, groupName]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[firstName, lastName, groupName]")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/StatusCondition.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupPartyName", location = "component://party/widget/partymgr/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupPartyName", location = "component://party/widget/partymgr/LookupForms.xml"
            )})
        }
    )
    public interface LookupTaxAuthorityPartyName {}

    @Screen(name = "LookupOrderPaymentPreference", location = "component://accounting/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"acctgBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.AccountingLookupOrderPaymentPreference}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "OrderPaymentPreference")
    @Action(type = ActionType.SET, field = "searchFields", value = "[orderPaymentPreferenceId, orderId, paymentMethodTypeId, paymentMethodId, statusId]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[orderPaymentPreferenceId, orderId, maxAmount, statusId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupOrderPaymentPreference", location = "component://accounting/widget/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listOrderPaymentPreference", location = "component://accounting/widget/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupOrderPaymentPreference {}

}
