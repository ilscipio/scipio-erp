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
public class SettingsGlSetupForms {

    @Form(
        name = "ListCompanies",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        title = "Internal Organizations",
        listName = "parties",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingCompanies}", displayEntity = @DisplayEntityField(entityName = "PartyGroup", description = "${groupName}")),
            @FormField(name = "setupAction", title = " ", useWhen = "hasPrefPermission", widgetStyle = "${styles.link_nav} ${styles.action_configure}", hyperlink = @HyperlinkField(target = "AdminMain", description = "${uiLabelMap.AccountingSetup}", parameters = {@ParameterDef(paramName = "organizationPartyId", fromField = "partyId")})),
            @FormField(name = "accounting", title = " ", useWhen = "hasBasicPermission", widgetStyle = "${styles.link_nav}", hyperlink = @HyperlinkField(target = "PartyAccountsSummary", description = "${uiLabelMap.AccountingAccounting}", parameters = {@ParameterDef(paramName = "organizationPartyId", fromField = "partyId")})),
            @FormField(name = "importexport", title = " ", useWhen = "hasBasicPermission", widgetStyle = "${styles.link_nav} ${styles.action_import}", hyperlink = @HyperlinkField(target = "ImportExport", description = "${uiLabelMap.CommonImportExport}", parameters = {@ParameterDef(paramName = "organizationPartyId", fromField = "partyId")}))
        }
    )
    public interface ListCompanies {}

    @Form(
        name = "ListGlAccountOrganization",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListGlAccountOrganization",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 50,
        fields = {
            @FormField(name = "accountCode", title = "${uiLabelMap.CommonCode}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${accountCode}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "accountName", entryName = "glAccountId", title = "${uiLabelMap.CommonName}", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountName}")),
            @FormField(name = "parentGlAccountId", title = "${uiLabelMap.CommonParent}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${parentGlAccountId}", useWhen = "parentGlAccountId!=null", parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.CommonClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass")),
            @FormField(name = "glResourceTypeId", title = "${uiLabelMap.CommonResource}", displayEntity = @DisplayEntityField(entityName = "GlResourceType"))
        }
    )
    public interface ListGlAccountOrganization {}

    @Form(
        name = "AddCompany",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "AdminMain",
        fields = {
            @FormField(name = "organizationPartyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName}[${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingNewCompany}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCompany {}

    @Form(
        name = "ExportInvoice",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "ExportInvoiceCsv.csv",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${parameters.organizationPartyId}")),
            @FormField(name = "invoiceId", lookup = @LookupField(targetFormName = "LookupInvoice")),
            @FormField(name = "startDate", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ExportInvoice {}

    @Form(
        name = "ExportInvoiceCsv",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "false",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 99999,
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceId", title = "invoiceId", display = @DisplayField),
            @FormField(name = "invoiceTypeId", title = "invoiceTypeId", display = @DisplayField),
            @FormField(name = "invoiceDate", title = "invoiceDate", display = @DisplayField),
            @FormField(name = "dueDate", title = "dueDate", display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "partyIdFrom", display = @DisplayField),
            @FormField(name = "partyIdFromTrans", title = "partyIdFromTrans", display = @DisplayField),
            @FormField(name = "partyId", title = "partyId", display = @DisplayField),
            @FormField(name = "partyIdTrans", title = "partyIdTrans", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "currencyUomId", display = @DisplayField),
            @FormField(name = "description", title = "description", display = @DisplayField),
            @FormField(name = "referenceNumber", title = "referenceNumber", display = @DisplayField),
            @FormField(name = "invoiceItemSeqId", title = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "invoiceItemTypeId", title = "invoiceItemTypeId", display = @DisplayField),
            @FormField(name = "productId", title = "productId", display = @DisplayField),
            @FormField(name = "productIdTrans", title = "productIdTrans", display = @DisplayField),
            @FormField(name = "itemDescription", title = "itemDescription", display = @DisplayField),
            @FormField(name = "quantity", title = "quantity", display = @DisplayField),
            @FormField(name = "amount", title = "amount", display = @DisplayField)
        }
    )
    public interface ExportInvoiceCsv {}

    @Form(
        name = "ImportInvoice",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.UPLOAD,
        target = "ImportInvoice",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${parameters.organizationPartyId}")),
            @FormField(name = "uploadedFile", file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface ImportInvoice {}

    @Form(
        name = "AssignGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createGlAccountOrganization",
        defaultMapName = "account",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} - ${accountName} [${glAccountId}]", orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingCreateAssignment}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AssignGlAccount {}

    @Form(
        name = "PartyAcctgPreference",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createPartyAcctgPreference",
        defaultMapName = "aggregatedPartyAcctgPreference",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyAcctgPreference")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${parameters.organizationPartyId}")),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingOrganizationPartyId}", display = @DisplayField),
            @FormField(name = "fiscalYearStartMonth", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('fiscalYearStartMonth')!=null)return (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(options = {@Option(key = "1", description = "${uiLabelMap.AccountingFiscalMonth01}"), @Option(key = "2", description = "${uiLabelMap.AccountingFiscalMonth02}"), @Option(key = "3", description = "${uiLabelMap.AccountingFiscalMonth03}"), @Option(key = "4", description = "${uiLabelMap.AccountingFiscalMonth04}"), @Option(key = "5", description = "${uiLabelMap.AccountingFiscalMonth05}"), @Option(key = "6", description = "${uiLabelMap.AccountingFiscalMonth06}"), @Option(key = "7", description = "${uiLabelMap.AccountingFiscalMonth07}"), @Option(key = "8", description = "${uiLabelMap.AccountingFiscalMonth08}"), @Option(key = "9", description = "${uiLabelMap.AccountingFiscalMonth09}"), @Option(key = "10", description = "${uiLabelMap.AccountingFiscalMonth10}"), @Option(key = "11", description = "${uiLabelMap.AccountingFiscalMonth11}"), @Option(key = "12", description = "${uiLabelMap.AccountingFiscalMonth12}")})),
            @FormField(name = "fiscalYearStartDay", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('fiscalYearStartDay')!=null)return (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(options = {@Option(key = "1"), @Option(key = "2"), @Option(key = "3"), @Option(key = "4"), @Option(key = "5"), @Option(key = "6"), @Option(key = "7"), @Option(key = "8"), @Option(key = "9"), @Option(key = "10"), @Option(key = "11"), @Option(key = "12"), @Option(key = "13"), @Option(key = "14"), @Option(key = "15"), @Option(key = "16"), @Option(key = "17"), @Option(key = "18"), @Option(key = "19"), @Option(key = "20"), @Option(key = "21"), @Option(key = "22"), @Option(key = "23"), @Option(key = "24"), @Option(key = "25"), @Option(key = "26"), @Option(key = "27"), @Option(key = "28"), @Option(key = "29"), @Option(key = "30"), @Option(key = "31")})),
            @FormField(name = "taxFormId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('taxFormId')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "TAX_FORMS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "cogsMethodId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('cogsMethodId')!=null)return (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "COGS_METHODS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "baseCurrencyUomId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('baseCurrencyUomId')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "invoiceIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('invoiceIdPrefix')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "oldInvoiceSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldInvoiceSequenceEnumId')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "INVOICE_SEQMD")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useInvoiceIdForReturns", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('useInvoiceIdForReturns')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "quoteIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('quoteIdPrefix')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "oldQuoteSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldQuoteSequenceEnumId')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "QUOTE_SEQMD")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "orderIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('orderIdPrefix')!=null)return (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "oldOrderSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties; if(aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldOrderSequenceEnumId')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SEQMD")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonAdd}", useWhen = "partyAcctgPreference==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyAcctgPreference!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "fiscalYearStartMonth", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('fiscalYearStartMonth')==null&&aggregatedPartyAcctgPreference.get('fiscalYearStartMonth')!=null)return             (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "fiscalYearStartDay", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;             if(partyAcctgPreference!= null&&partyAcctgPreference.get('fiscalYearStartDay')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('fiscalYearStartDay')!=null)return                     (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "taxFormId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('taxFormId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('taxFormId')!=null)return                     (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "cogsMethodId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;             if(partyAcctgPreference!= null&&partyAcctgPreference.get('cogsMethodId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('cogsMethodId')!=null)return                     (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "baseCurrencyUomId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('baseCurrencyUomId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('baseCurrencyUomId')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "invoiceIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('invoiceIdPrefix')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('invoiceIdPrefix')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "oldInvoiceSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('oldInvoiceSequenceEnumId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldInvoiceSequenceEnumId')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "useInvoiceIdForReturns", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('useInvoiceIdForReturns')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('useInvoiceIdForReturns')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "oldQuoteSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('oldQuoteSequenceEnumId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldQuoteSequenceEnumId')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "quoteIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('quoteIdPrefix')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('quoteIdPrefix')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "lastQuoteNumber", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('lastQuoteNumber')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastQuoteNumber')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "oldOrderSequenceEnumId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('oldOrderSequenceEnumId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('oldOrderSequenceEnumId')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "orderIdPrefix", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('orderIdPrefix')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('orderIdPrefix')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "lastOrderNumber", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if(partyAcctgPreference!= null&&partyAcctgPreference.get('lastOrderNumber')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastOrderNumber')!=null)return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", useWhen = "partyAcctgPreference!=null", display = @DisplayField),
            @FormField(name = "lastInvoiceNumber", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if((partyAcctgPreference==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastInvoiceNumber')!=null) ||                 (partyAcctgPreference!=null&&partyAcctgPreference.get('lastInvoiceNumber')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastInvoiceNumber')!=null))return                     (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", display = @DisplayField),
            @FormField(name = "lastInvoiceRestartDate", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if((partyAcctgPreference==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastInvoiceRestartDate')!=null) ||                 (partyAcctgPreference!=null&&partyAcctgPreference.get('lastInvoiceRestartDate')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('lastInvoiceRestartDate')!=null))return (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", display = @DisplayField),
            @FormField(name = "refundPaymentMethodId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;                 if((partyAcctgPreference==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('refundPaymentMethodId')!=null) ||                 (partyAcctgPreference!=null&&partyAcctgPreference.get('refundPaymentMethodId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('refundPaymentMethodId')!=null))return                 (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethod", description = "${description}", keyFieldName = "paymentMethodId", constraints = {@EntityConstraint(name = "partyId", envName = "organizationPartyId")}))),
            @FormField(name = "errorGlJournalId", tooltip = "${groovy: import org.ofbiz.base.util.UtilProperties;             if((partyAcctgPreference==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('errorGlJournalId')!=null) ||             (partyAcctgPreference!=null&&partyAcctgPreference.get('errorGlJournalId')==null&&aggregatedPartyAcctgPreference!= null&&aggregatedPartyAcctgPreference.get('errorGlJournalId')!=null))return              (UtilProperties.getMessage('AccountingUiLabels', 'AccountingInheritedValue', locale))}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlJournal", description = "${glJournalName} [${glJournalId}]", keyFieldName = "glJournalId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glJournalName")})))
        },
        altTargets = {
            @AltTarget(useWhen = "partyAcctgPreference!=null", target = "updatePartyAcctgPreference")
        },
        actions = @FormActions(set = {@SetAction(field = "aggregatedPartyAcctgPreference", fromField = "result.partyAccountingPreference", type = "Object")}, service = {@ServiceAction(serviceName = "getPartyAccountingPreferences", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "organizationPartyId")})})
    )
    public interface PartyAcctgPreference {}

    @Form(
        name = "ListConversions",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "conversions",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "uomId", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "uomIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "purposeEnumId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "conversionFactor", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField)
        }
    )
    public interface ListConversions {}

    @Form(
        name = "updateFXConversion",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "updateFXConversion",
        defaultServiceName = "updateFXConversion",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "uomIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "purposeEnumId", title = "${uiLabelMap.CommonPurpose}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CONVERSION_PURPOSE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "conversionFactor", text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingUpdateFX}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface updateFXConversion {}

    @Form(
        name = "ListGlAccountTypeDefaults",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "glAccountTypeDefaults",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glAccountTypeId", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", description = "${accountCode} ${accountName}", subHyperlink = @SubHyperlink(target = "GlAccountNavigate", description = "${glAccountId}", parameters = {@ParameterDef(paramName = "glAccountId")}))),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeGlAccountTypeDefault", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "glAccountTypeId"), @ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface ListGlAccountTypeDefaults {}

    @Form(
        name = "EditGlAccountTypeDefault",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createGlAccountTypeDefault",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createGlAccountTypeDefault")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditGlAccountTypeDefault {}

    @Form(
        name = "ListSalInvoiceItemTypeGlAssignments",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "invoiceItemTypes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceItemTypeId", hidden = @HiddenField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "defaultGlAccountId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${defaultGlAccountId}", parameters = {@ParameterDef(paramName = "glAccountId", fromField = "defaultGlAccountId")})),
            @FormField(name = "overrideGlAccountId", display = @DisplayField),
            @FormField(name = "activeGlDescription", display = @DisplayField),
            @FormField(name = "removeAction", title = " ", useWhen = "defaultAccount==false", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeSalInvoiceItemTypeGlAssignment", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "invoiceItemTypeId")}))
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/admin/ListInvoiceItemTypesGlAccount.groovy")})
    )
    public interface ListSalInvoiceItemTypeGlAssignments {}

    @Form(
        name = "AddSalInvoiceItemTypeGlAssignment",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "addSalInvoiceItemTypeGlAssignment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addInvoiceItemTypeGlAssignment")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceItemTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InvoiceItemType", description = "${description}", constraints = {@EntityConstraint(name = "invoiceItemTypeId", value = "INV_%", operator = "like")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingOverrideRevenueGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", envName = "revenueAccountClassIds", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "revenueAccountClassIds", value = "${groovy:org.ofbiz.accounting.util.UtilAccounting.getDescendantGlAccountClassIds(revenueGlAccountClass)}", type = "List")}, entityOne = {@EntityOneAction(entityName = "GlAccountClass", valueField = "revenueGlAccountClass")})
    )
    public interface AddSalInvoiceItemTypeGlAssignment {}

    @Form(
        name = "ListPurInvoiceItemTypeGlAssignments",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "invoiceItemTypes",
        defaultMapName = "iTypes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceItemTypeId", hidden = @HiddenField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "defaultGlAccountId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${defaultGlAccountId}", parameters = {@ParameterDef(paramName = "glAccountId", fromField = "defaultGlAccountId")})),
            @FormField(name = "overrideGlAccountId", title = "${uiLabelMap.AccountingInvoiceOverrideExpenseGlAccountId}", display = @DisplayField),
            @FormField(name = "activeGlDescription", display = @DisplayField),
            @FormField(name = "removeAction", title = " ", useWhen = "defaultAccount==false", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePurInvoiceItemTypeGlAssignment", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "invoiceItemTypeId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "invItemTypePrefix", value = "PINV")}, script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/admin/ListInvoiceItemTypesGlAccount.groovy")})
    )
    public interface ListPurInvoiceItemTypeGlAssignments {}

    @Form(
        name = "AddPurInvoiceItemTypeGlAssignment",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "addPurInvoiceItemTypeGlAssignment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addInvoiceItemTypeGlAssignment")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceItemTypeId", title = "${uiLabelMap.AccountingInvoicePurchaseItemType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InvoiceItemType", description = "${description}", constraints = {@EntityConstraint(name = "invoiceItemTypeId", value = "PINV%", operator = "like")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingInvoiceOverrideExpenseGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", envName = "expenseAccountClassIds", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "expenseAccountClassIds", value = "${groovy:org.ofbiz.accounting.util.UtilAccounting.getDescendantGlAccountClassIds(expenseGlAccountClass)}", type = "List")}, entityOne = {@EntityOneAction(entityName = "GlAccountClass", valueField = "expenseGlAccountClass")})
    )
    public interface AddPurInvoiceItemTypeGlAssignment {}

    @Form(
        name = "ListPaymentTypeGlAssignments",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGlAccountTypeMap", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.AccountingPaymentType}", displayEntity = @DisplayEntityField(entityName = "PaymentType")),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePaymentTypeGlAssignment", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "paymentTypeId")}))
        }
    )
    public interface ListPaymentTypeGlAssignments {}

    @Form(
        name = "AddPaymentTypeGlAssignment",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "addPaymentTypeGlAssignment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addPaymentTypeGlAssignment")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.AccountingPaymentType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", keyFieldName = "paymentTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPaymentTypeGlAssignment {}

    @Form(
        name = "ListPaymentMethodTypeGlAssignments",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentMethodTypeGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "paymentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType")),
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", description = "${accountName}", subHyperlink = @SubHyperlink(target = "GlAccountNavigate", description = "[${glAccountId}]", parameters = {@ParameterDef(paramName = "glAccountId")}))),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "defaultGlAccountId", widgetStyle = "${styles.link_nav_info_desc}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${defaultGlAccountId} : ${description}", parameters = {@ParameterDef(paramName = "glAccountId", fromField = "defaultGlAccountId")})),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePaymentMethodTypeGlAssignment", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "paymentMethodTypeId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "defaultGlAccountId", value = "${paymentMethodType.defaultGlAccountId}"), @SetAction(field = "description", value = "${glAccount.description}")}, entityOne = {@EntityOneAction(entityName = "PaymentMethodType", valueField = "paymentMethodType"), @EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface ListPaymentMethodTypeGlAssignments {}

    @Form(
        name = "AddPaymentMethodTypeGlAssignment",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "addPaymentMethodTypeGlAssignment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addPaymentMethodTypeGlAssignment")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "paymentMethodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", keyFieldName = "paymentMethodTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPaymentMethodTypeGlAssignment {}

    @Form(
        name = "CreateTimePeriod",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createCustomTimePeriod",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCustomTimePeriod")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "isClosed", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "parentPeriodId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "openTimePeriods", keyName = "customTimePeriodId", description = "[${customTimePeriodId}] ${periodName}: ${fromDate} - ${thruDate}"))),
            @FormField(name = "periodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateTimePeriod {}

    @Form(
        name = "EditGlJournal",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createGlJournal",
        defaultMapName = "glJournal",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createGlJournal")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "glJournalId", title = "${uiLabelMap.CommonId}", useWhen = "glJournal!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "glJournal!=null", target = "updateGlJournal")
        }
    )
    public interface EditGlJournal {}

    @Form(
        name = "ListGlJournals",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "GlJournal", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glJournalId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "SetupGlJournals", description = "${glJournalId}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "glJournalId")})),
            @FormField(name = "glJournalName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteGlJournal", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "glJournalId")}))
        }
    )
    public interface ListGlJournals {}

    @Form(
        name = "ListProductGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateProductGlAccount",
        listName = "productGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", display = @DisplayField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductGlAccount", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "glAccountTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductGlAccounts {}

    @Form(
        name = "AddProductGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createProductGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductGlAccount {}

    @Form(
        name = "ListFinAccountTypeGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateFinAccountTypeGlAccount",
        listName = "finAccountTypeGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFinAccountTypeGlAccount")
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FinAccountType")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFinAccountTypeGlAccount", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "finAccountTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListFinAccountTypeGlAccounts {}

    @Form(
        name = "AddFinAccountTypeGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createFinAccountTypeGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "finAccountTypes", keyName = "finAccountTypeId", description = "${description}"))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFinAccountTypeGlAccount {}

    @Form(
        name = "ListProductCategoryGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryGlAccount",
        listName = "productCategoryGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductCategoryGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductCategoryGlAccount", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "glAccountTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductCategoryGlAccounts {}

    @Form(
        name = "AddProductCategoryGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createProductCategoryGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.Type}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductProductCategoryId}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductCategoryGlAccount {}

    @Form(
        name = "ListVarianceReasonGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateVarianceReasonGlAccount",
        listName = "varianceReasonGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateVarianceReasonGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "varianceReasonId", displayEntity = @DisplayEntityField(entityName = "VarianceReason")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteVarianceReasonGlAccount", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "varianceReasonId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListVarianceReasonGlAccounts {}

    @Form(
        name = "AddVarianceReasonGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createVarianceReasonGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "varianceReasonId", title = "${uiLabelMap.FormFieldTitle_varianceReasonId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "VarianceReason", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "varianceReasonId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.FormFieldTitle_glAccountTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddVarianceReasonGlAccount {}

    @Form(
        name = "ListCreditCardTypeGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateCreditCardTypeGlAccount",
        listName = "creditCardTypeGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCreditCardTypeGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCreditCardTypeGlAccount", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "cardType")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListCreditCardTypeGlAccounts {}

    @Form(
        name = "AddCreditCardTypeGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createCreditCardTypeGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "cardType", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${enumCode}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CREDIT_CARD_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "enumId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCreditCardTypeGlAccount {}

    @Form(
        name = "ListTaxAuthorityGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateOrganizationTaxAuthorityGlAccount",
        listName = "taxAuthorityGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTaxAuthorityGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "[${geoId}] ${geoName}")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${taxAuthPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "taxAuthPartyId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteOrganizationTaxAuthorityGlAccount", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "taxAuthPartyId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListTaxAuthorityGlAccounts {}

    @Form(
        name = "AddTaxAuthorityGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "createOrganizationTaxAuthorityGlAccount",
        listName = "taxAuthorityHavingNoGlAccountList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "[${geoId}] ${geoName}")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${taxAuthPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "taxAuthPartyId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTaxAuthorityGlAccount {}

    @Form(
        name = "ListPartyGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updatePartyGlAccount",
        listName = "partyGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", display = @DisplayField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyGlAccount", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "glAccountTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListPartyGlAccounts {}

    @Form(
        name = "AddPartyGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createPartyGlAccount",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyGlAccount", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyGlAccount {}

    @Form(
        name = "ListGlAccountOrgCsv",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "false",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 99999,
        fields = {
            @FormField(name = "glAccountId", display = @DisplayField(description = "${glAccountId}")),
            @FormField(name = "glAccountTypeId", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", displayEntity = @DisplayEntityField(entityName = "GlAccountClass", keyFieldName = "glAccountClassId")),
            @FormField(name = "glResourceTypeId", displayEntity = @DisplayEntityField(entityName = "GlResourceType", keyFieldName = "glResourceTypeId")),
            @FormField(name = "glXbrlClassId", displayEntity = @DisplayEntityField(entityName = "GlXbrlClass", keyFieldName = "glXbrlClassId")),
            @FormField(name = "parentGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${parentGlAccountId}")),
            @FormField(name = "accountCode", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode}")),
            @FormField(name = "accountName", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountName}")),
            @FormField(name = "description", display = @DisplayField(description = "${description}")),
            @FormField(name = "productId", display = @DisplayField(description = "${productId}"))
        }
    )
    public interface ListGlAccountOrgCsv {}

    @Form(
        name = "FindGlAccountCategory",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "FindGlAccountCategory",
        defaultEntityName = "GlAccountCategory",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "glAccountCategoryId", textFind = @TextFindField),
            @FormField(name = "glAccountCategoryTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountCategoryType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountCategoryType", description = "${description}", keyFieldName = "glAccountCategoryTypeId"))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindGlAccountCategory {}

    @Form(
        name = "ListGlAccountCategory",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "GlAccountCategory",
        paginateTarget = "FindGlAccountCategory",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "glAccountCategoryId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditGlAccountCategory", description = "${glAccountCategoryId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountCategoryId"), @ParameterDef(paramName = "glAccountCategoryTypeId")})),
            @FormField(name = "glAccountCategoryTypeId", displayEntity = @DisplayEntityField(entityName = "GlAccountCategoryType")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "GlAccountCategory"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListGlAccountCategory {}

    @Form(
        name = "EditGlAccountCategory",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "updateGlAccountCategory",
        defaultMapName = "glAccountCategory",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "GlAccountCategory", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "glAccountCategoryId", useWhen = "glAccountCategoryId!=null", display = @DisplayField),
            @FormField(name = "glAccountCategoryId", useWhen = "glAccountCategoryId==null", hidden = @HiddenField),
            @FormField(name = "glAccountCategoryTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountCategoryType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountCategoryType", description = "${description}", keyFieldName = "glAccountCategoryTypeId"))),
            @FormField(name = "submit", title = "${uiLabelMap.CommonCreate}", useWhen = "glAccountCategory==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonUpdate}", useWhen = "glAccountCategory!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "glAccountCategory==null", target = "createGlAccountCategory")
        }
    )
    public interface EditGlAccountCategory {}

    @Form(
        name = "ListGlAccountCategoryMember",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        target = "updateGlAccountCategoryMember",
        listName = "glAccountCategoryMemberList",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "GlAccountCategoryMember", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${glAccountId}-${accountName}")),
            @FormField(name = "glAccountCategoryId", displayEntity = @DisplayEntityField(entityName = "GlAccountCategory")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteGlAccountCategoryMember", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountId", fromField = "glAccountId"), @ParameterDef(paramName = "glAccountCategoryId", fromField = "glAccountCategoryId"), @ParameterDef(paramName = "fromDate", fromField = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListGlAccountCategoryMember {}

    @Form(
        name = "AddGlAccountCategoryMember",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createGlAccountCategoryMember",
        fields = {
            @FormField(name = "glAccountCategoryId", display = @DisplayField),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccount", description = "${glAccountId}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "amountPercentage", text = @TextField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddGlAccountCategoryMember {}

    @Form(
        name = "AddFixedAssetTypeGlAccount",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        target = "createFixedAssetTypeGlAccount",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFixedAssetTypeGlAccount", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "fixedAssetId", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonAll}")}, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetId} - ${fixedAssetName}", constraints = {@EntityConstraint(name = "partyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "fixedAssetId")}))),
            @FormField(name = "fixedAssetTypeId", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonAll}")}, entityOptions = @EntityOptions(entityName = "FixedAssetType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "assetGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", value = "LONGTERM_ASSET")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "accDepGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", value = "ACCUM_DEPRECIATION")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "depGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", value = "DEPRECIATION")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "profitGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", value = "CASH_INCOME")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "lossGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${organizationPartyId}"), @EntityConstraint(name = "glAccountClassId", value = "SGA_EXPENSE")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetTypeGlAccount {}

    @Form(
        name = "ListFixedAssetTypeGlAccounts",
        location = "component://accounting/widget/settings/GlSetupForms.xml",
        type = FormType.LIST,
        listName = "fixedAssetTypeGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetType")),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.CommonFixedAsset}", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetId} ${fixedAssetName}")),
            @FormField(name = "assetGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "accDepGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "depGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "profitGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "lossGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetTypeGlAccount", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetTypeId"), @ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface ListFixedAssetTypeGlAccounts {}

}
