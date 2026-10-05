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
public class FinanceFinAccountForms {

    @Form(
        name = "FindFinAccounts",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindFinAccount",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FinAccount", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "finAccountId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FinAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "finAccountCode", textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "displayAdvancedSearch", hidden = @HiddenField(value = "true")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFinAccounts {}

    @Form(
        name = "ListFinAccounts",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "FinAccount",
        paginate = "true",
        paginateTarget = "FindFinAccount",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "finAccountId", title = "${uiLabelMap.CommonName}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditFinAccount", description = "${finAccountName} ${finAccountCode}", parameters = {@ParameterDef(paramName = "finAccountId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FinAccountType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "currencyUomId", display = @DisplayField),
            @FormField(name = "actualBalance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonOrganisation}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${organizationPartyId}", parameters = {@ParameterDef(paramName = "partyId", fromField = "organizationPartyId")})),
            @FormField(name = "ownerPartyId", title = "${uiLabelMap.CommonOwner}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${ownerPartyId}", parameters = {@ParameterDef(paramName = "partyId", fromField = "ownerPartyId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFinAccount", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "finAccountId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FinAccount"), @FieldMap(fieldName = "orderBy", value = "finAccountId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFinAccounts {}

    @Form(
        name = "EditFinAccount",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "updateFinAccount",
        defaultMapName = "finAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "finAccountId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "finAccountId!=null", display = @DisplayField),
            @FormField(name = "finAccountId", useWhen = "finAccount==null&&finAccountId==null", ignored = @IgnoredField),
            @FormField(name = "finAccountId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${finAccountId}]", useWhen = "finAccount==null&&finAccountId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FinAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "finAccountCode", text = @TextField(size = 20)),
            @FormField(name = "finAccountPin", position = 2, text = @TextField(size = 10)),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "finAccount==null", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FINACCT_STATUS")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "finAccount!=null", position = 2, dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", envName = "finAccount.statusId")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonOrganisation}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "ownerPartyId", title = "${uiLabelMap.CommonOwner}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "isRefundable", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "postToGlAccountId", position = 2, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "replenishPaymentId", text = @TextField),
            @FormField(name = "replenishLevel", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "finAccountId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "finAccountId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "actualBalance", useWhen = "finAccount!=null", display = @DisplayField(type = "currency")),
            @FormField(name = "availableBalance", useWhen = "finAccount!=null", position = 2, display = @DisplayField(type = "currency"))
        },
        altTargets = {
            @AltTarget(useWhen = "finAccount==null", target = "createFinAccount")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditFinAccount {}

    @Form(
        name = "ListFinAccountRoles",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        target = "updateFinAccountRole",
        listName = "finAccountRoles",
        paginateTarget = "EditFinAccountRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${partyId} - ${party.groupName} ${party.firstName} ${party.lastName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFinAccountRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "finAccountId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListFinAccountRoles {}

    @Form(
        name = "AddFinAccountRole",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createFinAccountRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFinAccountRole")
        },
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFinAccountRole {}

    @Form(
        name = "AddFinAccountTrans",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createFinAccountTrans",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFinAccountTrans")
        },
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "finAccountTransId", hidden = @HiddenField),
            @FormField(name = "finAccountTransTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FinAccountTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "transactionDate", dateTime = @DateTimeField),
            @FormField(name = "entryDate", dateTime = @DateTimeField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "orderId", title = "${uiLabelMap.CommonOrder}", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "orderItemSeqId", text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FINACT_TRNS_STATUS")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccount}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "defaultOrganizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glAccountId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "statusId", value = "FINACT_TRNS_CREATED")})
    )
    public interface AddFinAccountTrans {}

    @Form(
        name = "ListFinAccountAuths",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        target = "expireFinAccountAuth",
        listName = "finAccountauths",
        paginateTarget = "EditFinAccountAuths",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FinAccountAuth", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "finAccountAuthId", display = @DisplayField),
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "expireAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "expireFinAccountAuth", description = "${uiLabelMap.CommonExpire}", alsoHidden = false, parameters = {@ParameterDef(paramName = "finAccountId"), @ParameterDef(paramName = "finAccountAuthId")}))
        }
    )
    public interface ListFinAccountAuths {}

    @Form(
        name = "AddFinAccountAuth",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createFinAccountAuth",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFinAccountAuth")
        },
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "finAccountAuthId", hidden = @HiddenField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField),
            @FormField(name = "authorizationDate", dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFinAccountAuth {}

    @Form(
        name = "PaymentsDepositWithdraw",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindPaymentsForDepositOrWithdraw",
        extendsForm = "FindBatchPayments",
        extendsResource = "component://accounting/widget/payments/PaymentForms.xml",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface PaymentsDepositWithdraw {}

    @Form(
        name = "FindDepositSlips",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindDepositSlips",
        extendsForm = "FindPaymentGroup",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField),
            @FormField(name = "paymentGroupTypeId", hidden = @HiddenField(value = "BATCH_PAYMENT"))
        }
    )
    public interface FindDepositSlips {}

    @Form(
        name = "ListDepositSlips",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        target = "FindDepositSlips",
        listName = "paymentGroupList",
        extendsForm = "ListPaymentGroup",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        paginateTarget = "FindDepositSlips",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentGroupId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditDepositSlipAndMembers", description = "${paymentGroupId}", parameters = {@ParameterDef(paramName = "paymentGroupId"), @ParameterDef(paramName = "finAccountId")})),
            @FormField(name = "deleteAction", title = " ", useWhen = "${paymentGroupTypeId == 'BATCH_PAYMENT'} @and ${paymentGroupMemberAndTransList[0].finAccountTransStatusId != 'FINACT_TRNS_APPROVED'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isNotEmpty(paymentGroupMembers)}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteDepositSlip", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentGroupId"), @ParameterDef(paramName = "finAccountId"), @ParameterDef(paramName = "glReconciliationId")}))
        }
    )
    public interface ListDepositSlips {}

    @Form(
        name = "EditDepositSlip",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "updateDepositSlip",
        extendsForm = "EditPaymentGroup",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        fields = {
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentGroupType")),
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}"))
        }
    )
    public interface EditDepositSlip {}

    @Form(
        name = "AddDepositSlipMember",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "addDepositSlipMember",
        extendsForm = "AddPaymentGroupMember",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}"))
        }
    )
    public interface AddDepositSlipMember {}

    @Form(
        name = "ListDepositSlipMember",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        target = "updateDepositSlipMember",
        extendsForm = "ListPaymentGroupMember",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        paginateTarget = "EditDepositSlipAndMembers",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "expireDepositSlipMember", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentGroupId"), @ParameterDef(paramName = "paymentId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "finAccountId")})),
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}"))
        }
    )
    public interface ListDepositSlipMember {}

    @Form(
        name = "QuickFindFinAccounts",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindFinAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "finAccountId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "finAccountTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FinAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "finAccountName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface QuickFindFinAccounts {}

    @Form(
        name = "FindFinAccountTransactions",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindFinAccountTrans",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}")),
            @FormField(name = "finAccountTransId", textFind = @TextFindField),
            @FormField(name = "finAccountTransTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FinAccountTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FINACT_TRNS_STATUS")}))),
            @FormField(name = "glReconciliationId", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonNotAssigned}")}, listOptions = @ListOptions(listName = "glReconciliations", keyName = "glReconciliationId", description = "${glReconciliationName}[[${glReconciliationId}] [${reconciledDate}] [${reconciledBalance}]]"))),
            @FormField(name = "fromTransactionDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruTransactionDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fromEntryDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruEntryDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFinAccountTransactions {}

    @Form(
        name = "FindBankReconciliationFinAcctTrans",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "BankReconciliation",
        extendsForm = "FindFinAccountTransactions",
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "glReconciliationId", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "glReconciliations", keyName = "glReconciliationId", description = "${glReconciliationName}[[${glReconciliationId}] [${reconciledDate}] [${reconciledBalance}]]")))
        }
    )
    public interface FindBankReconciliationFinAcctTrans {}

    @Form(
        name = "EditDepositPayment",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createDepositPayment",
        extendsForm = "EditPayment",
        extendsResource = "component://accounting/widget/payments/PaymentForms.xml",
        fields = {
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", envName = "parentTypeId")}))),
            @FormField(name = "paymentMethodId", title = "${uiLabelMap.CommonMethod}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentMethod", description = "${paymentMethodTypeId} (${paymentMethodId})"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "partyIdTo", position = 2, lookup = @LookupField(targetFormName = "LookupInternalOrganization")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", position = 2, text = @TextField(size = 6)),
            @FormField(name = "comments", position = 2, text = @TextField(size = 35)),
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}")),
            @FormField(name = "finAccountTypeId", hidden = @HiddenField(value = "${finAccountTypeId}")),
            @FormField(name = "finAccountTransTypeId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", position = 2, hidden = @HiddenField(value = "${defaultOrganizationPartyCurrencyUomId}")),
            @FormField(name = "isDepositWithDrawPayment", title = "${uiLabelMap.AccountingDepositPaymentInFinAccount}", check = @CheckField),
            @FormField(name = "paymentGroupTypeId", hidden = @HiddenField(value = "BATCH_PAYMENT")),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", ignored = @IgnoredField),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "finAccountTypeId", fromField = "finAccount.finAccountTypeId")}, entityOne = {@EntityOneAction(entityName = "FinAccount", valueField = "finAccount")})
    )
    public interface EditDepositPayment {}

    @Form(
        name = "EditWithdrawalPayment",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createWithdrawalPayment",
        extendsForm = "EditDepositPayment",
        fields = {
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupInternalOrganization")),
            @FormField(name = "isDepositWithDrawPayment", title = "${uiLabelMap.AccountingWithdrawalPaymentInFinAccount}", check = @CheckField),
            @FormField(name = "paymentGroupTypeId", ignored = @IgnoredField)
        }
    )
    public interface EditWithdrawalPayment {}

    @Form(
        name = "EditFinAccountReconciliation",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "createGlReconciliation",
        defaultMapName = "glReconciliation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}")),
            @FormField(name = "glReconciliationId", useWhen = "glReconciliationId == null", ignored = @IgnoredField),
            @FormField(name = "glReconciliationId", useWhen = "glReconciliationId != null", display = @DisplayField),
            @FormField(name = "statusId", useWhen = "glReconciliationId == null", position = 2, hidden = @HiddenField(value = "GLREC_CREATED")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "glReconciliationId != null", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "GLREC_STATUS")}))),
            @FormField(name = "glReconciliationName", text = @TextField),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "glAccountId", hidden = @HiddenField(value = "${finAccount.postToGlAccountId}")),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonOrganisation}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "createdDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "lastModifiedDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "openingBalance", useWhen = "\"GLREC_RECONCILED\".equals(\"${glReconciliation.statusId}\")", display = @DisplayField),
            @FormField(name = "reconciledBalance", useWhen = "${reconciledBalance == null}", position = 2, hidden = @HiddenField),
            @FormField(name = "reconciledBalance", useWhen = "\"GLREC_RECONCILED\".equals(\"${glReconciliation.statusId}\")", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "reconciledDate", title = "${uiLabelMap.AccountingReconciliationDate}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "createAction", useWhen = "glReconciliationId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", useWhen = "glReconciliationId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "glReconciliationId != null", target = "updateFinAccountGlReconciliation")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "GlReconciliation", valueField = "glReconciliation")})
    )
    public interface EditFinAccountReconciliation {}

    @Form(
        name = "ListFinAccountReconciliations",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        type = FormType.LIST,
        listName = "glReconciliations",
        listEntryName = "glReconciliation",
        paginateTarget = "FindFinAccountReconciliations",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateGlReconciliation", mapName = "glReconciliation", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "glReconciliationId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewGlReconciliationWithTransaction", description = "${glReconciliation.glReconciliationId}", parameters = {@ParameterDef(paramName = "glReconciliationId", fromField = "glReconciliation.glReconciliationId"), @ParameterDef(paramName = "finAccountId")})),
            @FormField(name = "glReconciliationName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyName.firstName} ${partyName.lastName}${partyName.groupName} [${partyName.partyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyName.partyId")})),
            @FormField(name = "cancelAction", title = "${uiLabelMap.AccountingCancelBankReconciliation}", useWhen = "${glReconciliation.statusId == 'GLREC_CREATED'}", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "cancelReconciliation", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glReconciliationId", fromField = "glReconciliation.glReconciliationId"), @ParameterDef(paramName = "finAccountId")})),
            @FormField(name = "reconciledBalance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "openingBalance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "currencyUomId", fromField = "defaultOrganizationPartyCurrencyUomId")}, entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "partyName")})
    )
    public interface ListFinAccountReconciliations {}

    @Form(
        name = "FindBankReconciliation",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        target = "FindFinAccountReconciliations",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${parameters.finAccountId}")),
            @FormField(name = "glReconciliationId", lookup = @LookupField(targetFormName = "LookupGlReconciliation")),
            @FormField(name = "glReconciliationName", position = 2, text = @TextField),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "GLREC_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountId", position = 2, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindBankReconciliation {}

    @Form(
        name = "FinAccountReconciliationBalance",
        location = "component://accounting/widget/finance/FinAccountForms.xml",
        defaultMapName = "currentGlReconciliation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "glReconciliationName", display = @DisplayField),
            @FormField(name = "statusId", position = 2, displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "openingBalance", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "reconciledDate", display = @DisplayField),
            @FormField(name = "reconciledBalance", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "currentClosingBalance", position = 2, display = @DisplayField(description = "${currentClosingBalance}", type = "currency"))
        }
    )
    public interface FinAccountReconciliationBalance {}

}
