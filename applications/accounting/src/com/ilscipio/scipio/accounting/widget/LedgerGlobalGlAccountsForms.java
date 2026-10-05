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
public class LedgerGlobalGlAccountsForms {

    @Form(
        name = "ListGlAccountOrganization",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListGlAccountOrganization",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "GlAccountOrganization", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${glAccountId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "GlAccountOrganization"), @SetAction(field = "noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListGlAccountOrganization {}

    @Form(
        name = "AssignGlAccount",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        target = "createGlAccountOrganization",
        defaultMapName = "account",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} - ${accountName} [${glAccountId}]", orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "partyId", parameterName = "organizationPartyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${partyId}", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingCreateAssignment}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AssignGlAccount {}

    @Form(
        name = "ListGlAccount",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "GlAccount",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", title = "${uiLabelMap.CommonCode}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${accountCode}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "accountName", entryName = "glAccountId", title = "${uiLabelMap.CommonName}", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountName}")),
            @FormField(name = "parentGlAccountId", title = "${uiLabelMap.CommonParent}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${parentGlAccountId}", useWhen = "parentGlAccountId!=null", parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.CommonClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass")),
            @FormField(name = "glResourceTypeId", title = "${uiLabelMap.CommonResource}", displayEntity = @DisplayEntityField(entityName = "GlResourceType"))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "GlAccount"), @SetAction(field = "noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", value = "accountCode"), @FieldMap(fieldName = "noConditionFind", value = "Y")})})
    )
    public interface ListGlAccount {}

    @Form(
        name = "ListGlAccountPdf",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        extendsForm = "ListGlAccount",
        fields = {
            @FormField(name = "accountCode", title = "${uiLabelMap.CommonCode}", widgetStyle = "buttontext", display = @DisplayField)
        }
    )
    public interface ListGlAccountPdf {}

    @Form(
        name = "EditGlAccount",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        target = "updateGlAccount",
        defaultMapName = "glAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonId}", useWhen = "glAccount!=null", display = @DisplayField),
            @FormField(name = "accountCode", title = "${uiLabelMap.CommonCode}", position = 2, text = @TextField),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", requiredField = true, text = @TextField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.CommonClass}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountClass", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glResourceTypeId", title = "${uiLabelMap.CommonResource}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlResourceType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glXbrlClassId", title = "XBRL ${uiLabelMap.CommonClass}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlXbrlClass", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "parentGlAccountId", title = "${uiLabelMap.CommonParent}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} - ${accountName}", keyFieldName = "glAccountId", orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Product", description = "${productId} - ${internalName}", orderBy = {@EntityOrderBy(fieldName = "productId")}))),
            @FormField(name = "externalId", position = 2, text = @TextField),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "glAccount==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "glAccount==null", target = "createGlAccount")
        }
    )
    public interface EditGlAccount {}

    @Form(
        name = "ListAcctgTransEntries",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        listName = "entries",
        defaultEntityName = "AcctgTransEntry",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ListAcctgTransEntries", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", description = "${accountName}", subHyperlink = @SubHyperlink(target = "ListGlAccountEntries", description = "[${glAccountId}]", parameters = {@ParameterDef(paramName = "glAccountId")}))),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "voucherRef", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "organizationPartyId", display = @DisplayField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "debitCreditFlag", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "reconcileStatusId", display = @DisplayField),
            @FormField(name = "settlementTermId", display = @DisplayField),
            @FormField(name = "isSummary", display = @DisplayField)
        }
    )
    public interface ListAcctgTransEntries {}

    @Form(
        name = "GlAccountsNavForm",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        title = "GL Accounts",
        defaultMapName = "journal",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "backToAdmin", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "AdminMain", description = "${uiLabelMap.AccountingBackToAdmin}", alsoHidden = false, parameters = {@ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface GlAccountsNavForm {}

    @Form(
        name = "ListGlReconciliations",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        listName = "glReconciliations",
        defaultEntityName = "GlReconciliation",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glReconciliationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditGlReconciliation", description = "${glReconciliationId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glReconciliationId"), @ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "glReconciliationName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", description = "${accountName}", subHyperlink = @SubHyperlink(target = "ListGlAccountEntries", description = "[${glAccountId}]", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")}))),
            @FormField(name = "reconciledBalance", display = @DisplayField),
            @FormField(name = "reconciledDate", display = @DisplayField)
        }
    )
    public interface ListGlReconciliations {}

    @Form(
        name = "ListGlReconciliationEntries",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        listName = "glReconciliationEntries",
        defaultEntityName = "GlReconciliationEntry",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glReconciliationId", display = @DisplayField),
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ListAcctgTransEntries", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "reconciledAmount", display = @DisplayField)
        }
    )
    public interface ListGlReconciliationEntries {}

    @Form(
        name = "ListRateAmounts",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        type = FormType.LIST,
        target = "expireRateAmount",
        paginateTarget = "viewRateAmounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "rateTypeId", hidden = @HiddenField),
            @FormField(name = "rateCurrencyUomId", hidden = @HiddenField),
            @FormField(name = "rateDescription", display = @DisplayField),
            @FormField(name = "periodTypeId", title = "${uiLabelMap.CommonPeriod}", display = @DisplayField(description = "${periodDescription}")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_name_long} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${groupName}${lastName} ${firstName} ${middleName}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "workEffortId", sortField = true, display = @DisplayField(description = "${workEffortName}")),
            @FormField(name = "emplPositionTypeId", title = "${uiLabelMap.CommonPosition}", sortField = true, display = @DisplayField(description = "${employeePositionDescription}")),
            @FormField(name = "rateAmount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "delete", title = "${uiLabelMap.CommonExpire}", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "sortField", fromField = "parameters.sortField", defaultValue = "rateTypeId")})
    )
    public interface ListRateAmounts {}

    @Form(
        name = "updateRateAmount",
        location = "component://accounting/widget/ledger/GlobalGlAccountsForms.xml",
        target = "updateRateAmount",
        defaultServiceName = "updateRateAmount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "rateTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "periodTypeId", title = "${uiLabelMap.CommonPeriod}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", constraints = {@EntityConstraint(name = "periodTypeId", value = "RATE_%", operator = "like")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "rateAmount", text = @TextField),
            @FormField(name = "rateCurrencyUomId", title = "${uiLabelMap.Currency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "emplPositionTypeId", title = "${uiLabelMap.CommonPosition}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplPositionType", description = "${description}", constraints = {@EntityConstraint(name = "emplPositionTypeId", value = "_NA_", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingUpdateRateAmount}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface updateRateAmount {}

}
