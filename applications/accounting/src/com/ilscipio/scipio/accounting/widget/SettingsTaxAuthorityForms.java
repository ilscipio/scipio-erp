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
public class SettingsTaxAuthorityForms {

    @Form(
        name = "FindTaxAuthority",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "FindTaxAuthority",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", lookup = @LookupField(targetFormName = "LookupTaxAuthorityGeo")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.CommonParty}", position = 2, lookup = @LookupField(targetFormName = "LookupTaxAuthorityPartyName")),
            @FormField(name = "requireTaxIdForExemption", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "includeTaxInPrice", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindTaxAuthority {}

    @Form(
        name = "originalFindTaxAuthority",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        listName = "taxAuthorityList",
        paginateTarget = "FindTaxAuthority",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "TaxAuthority", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "taxAuthGeoId", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "taxAuthPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${taxAuthPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "taxAuthPartyId")}))),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditTaxAuthority", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId")}))
        }
    )
    public interface originalFindTaxAuthority {}

    @Form(
        name = "ListTaxAuthorities",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindTaxAuthority",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditTaxAuthority", description = "${party.groupName} ${party.firstName} ${party.lastName} [${partyId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId")})),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "requireTaxIdForExemption", display = @DisplayField),
            @FormField(name = "taxIdFormatPattern", display = @DisplayField),
            @FormField(name = "includeTaxInPrice", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "TaxAuthorityAndDetail")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "partyId", fromField = "taxAuthPartyId")}, entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListTaxAuthorities {}

    @Form(
        name = "EditTaxAuthority",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "updateTaxAuthority",
        defaultMapName = "taxAuthority",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.CommonParty}", useWhen = "taxAuthority==null&&taxAuthPartyId==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.CommonParty}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "taxAuthority!=null", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.PartyParty}", tooltip = "${uiLabelMap.CommonCannotBeFound}:[${taxAuthPartyId}]", useWhen = "taxAuthority==null&&taxAuthPartyId!=null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", useWhen = "taxAuthority!=null", position = 2, displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", useWhen = "taxAuthority==null&&taxAuthGeoId==null", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", tooltip = "${uiLabelMap.CommonCannotBeFound}:[${taxAuthGeoId}]", useWhen = "taxAuthority==null&&taxAuthGeoId!=null", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "requireTaxIdForExemption", widgetStyle = "+smallSelect", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "includeTaxInPrice", widgetStyle = "+smallSelect", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "taxIdFormatPattern", tooltip = "${uiLabelMap.AccountingValidationPattern}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "taxAuthority!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "taxAuthority==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "taxAuthority==null", target = "createTaxAuthority")
        }
    )
    public interface EditTaxAuthority {}

    @Form(
        name = "ListTaxAuthorityCategories",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        target = "updateTaxAuthorityCategory",
        listName = "taxAuthorityCategoryList",
        paginateTarget = "EditTaxAuthorityCategories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonCategory}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "/catalog/control/EditCategory", urlMode = UrlMode.INTER_APP, description = "${productCategoryId} - ${category.categoryName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTaxAuthorityCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "productCategoryId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "ProductCategory", valueField = "category")})
    )
    public interface ListTaxAuthorityCategories {}

    @Form(
        name = "AddTaxAuthorityCategory",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "createTaxAuthorityCategory",
        defaultMapName = "taxAuthorityCategory",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTaxAuthorityCategory")
        },
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonCategory}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTaxAuthorityCategory {}

    @Form(
        name = "ListTaxAuthorityAssocs",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        target = "updateTaxAuthorityAssoc",
        listName = "taxAuthorityAssocList",
        paginateTarget = "EditTaxAuthorityAssocs",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthCombinedId", title = "${uiLabelMap.CommonTo} ${uiLabelMap.AccountingTaxAuthority}", hyperlink = @HyperlinkField(target = "EditTaxAuthority", description = "${party.groupName} ${party.firstName} ${party.lastName} [${partyId}] / ${geo.geoName} [${geo.geoId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId", fromField = "toTaxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId", fromField = "toTaxAuthGeoId")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "taxAuthorityAssocTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TaxAuthorityAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTaxAuthorityAssoc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "toTaxAuthPartyId"), @ParameterDef(paramName = "toTaxAuthGeoId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "partyId", fromField = "toTaxAuthPartyId"), @SetAction(field = "geoId", fromField = "toTaxAuthGeoId")}, entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party"), @EntityOneAction(entityName = "Geo", valueField = "geo")})
    )
    public interface ListTaxAuthorityAssocs {}

    @Form(
        name = "AddTaxAuthorityAssoc",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "createTaxAuthorityAssoc",
        defaultMapName = "taxAuthorityAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "toTaxAuthCombinedId", title = "${uiLabelMap.CommonTo} ${uiLabelMap.AccountingTaxAuthority}", widgetStyle = "+AddTaxAuthorityAssoc_toTaxAuthCombinedId_field", requiredField = true, dropDown = @DropDownField(listOptions = @ListOptions(listName = "taxAuthorityInfoList", keyName = "taxAuthCombinedId", description = "${party.groupName} ${party.firstName} ${party.lastName} [${taxAuthPartyId}] / ${geo.geoName} [${taxAuthGeoId}]"))),
            @FormField(name = "taxAuthorityAssocTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TaxAuthorityAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/tax/GetTaxAuthorityListForDisplay.groovy")})
    )
    public interface AddTaxAuthorityAssoc {}

    @Form(
        name = "ListTaxAuthorityGlAccounts",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        target = "updateTaxAuthorityGlAccount",
        listName = "taxAuthorityGlAccountList",
        paginateTarget = "EditTaxAuthorityGlAccounts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccount}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${glAccount.accountCode} - ${glAccount.accountName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${party.groupName} ${party.firstName} ${party.lastName} [${partyId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTaxAuthorityGlAccount", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "partyId", fromField = "organizationPartyId")}, entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount"), @EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListTaxAuthorityGlAccounts {}

    @Form(
        name = "AddTaxAuthorityGlAccount",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "createTaxAuthorityGlAccount",
        defaultMapName = "taxAuthorityGlAccount",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTaxAuthorityGlAccount")
        },
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.PartyOrganizationPartyId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccount}", requiredField = true, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTaxAuthorityGlAccount {}

    @Form(
        name = "ListTaxAuthorityRateProducts",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        target = "updateTaxAuthorityRateProduct",
        listName = "taxAuthorityRateProductList",
        paginateTarget = "EditTaxAuthorityRateProducts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTaxAuthorityRateProduct")
        },
        fields = {
            @FormField(name = "taxAuthorityRateSeqId", hidden = @HiddenField),
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthorityRateTypeId", title = "${uiLabelMap.CommonType}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TaxAuthorityRateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategory}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TaxAuthorityCategoryView", description = "${description} [${productCategoryId}]", constraints = {@EntityConstraint(name = "taxAuthPartyId", envName = "taxAuthPartyId"), @EntityConstraint(name = "taxAuthGeoId", envName = "taxAuthGeoId")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "titleTransferEnumId", title = "${uiLabelMap.AccountingTitleTransfer}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} [${enumCode}]", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PTSOFTTFR")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTaxAuthorityRateProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "taxAuthorityRateSeqId")}))
        }
    )
    public interface ListTaxAuthorityRateProducts {}

    @Form(
        name = "AddTaxAuthorityRateProduct",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "createTaxAuthorityRateProduct",
        defaultMapName = "taxAuthorityRateProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthorityRateTypeId", title = "${uiLabelMap.CommonType}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TaxAuthorityRateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productStoreId", title = "${uiLabelMap.CommonStore}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonCategory}", tooltip = "${uiLabelMap.AccountingTaxAuthorityRateProductUseCategoryTab}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TaxAuthorityCategoryView", description = "${description} [${productCategoryId}]", constraints = {@EntityConstraint(name = "taxAuthPartyId", envName = "taxAuthPartyId"), @EntityConstraint(name = "taxAuthGeoId", envName = "taxAuthGeoId")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "titleTransferEnumId", title = "${uiLabelMap.AccountingTitleTransfer}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} [${enumCode}]", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PTSOFTTFR")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "minItemPrice", text = @TextField),
            @FormField(name = "minPurchase", position = 2, text = @TextField),
            @FormField(name = "taxShipping", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "taxPromotions", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "taxPercentage", position = 2, text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTaxAuthorityRateProduct {}

    @Form(
        name = "FindTaxAuthorityParties",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "ListTaxAuthorityParties",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyTaxId", title = "${uiLabelMap.PartyTaxId}", position = 2, textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField),
            @FormField(name = "isExempt", widgetStyle = "+smallSelect", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isNexus", widgetStyle = "+smallSelect", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindTaxAuthorityParties {}

    @Form(
        name = "ListTaxAuthorityParties",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListTaxAuthorityParties",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditTaxAuthorityPartyInfo", description = "${party.groupName} ${party.firstName} ${party.lastName} [${partyId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "partyTaxId", title = "${uiLabelMap.PartyTaxId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "isExempt", display = @DisplayField),
            @FormField(name = "isNexus", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTaxAuthorityPartyInfo", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "entityName", value = "PartyTaxAuthInfo"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party"), @EntityOneAction(entityName = "Party", valueField = "taxAuthParty", autoFieldMap = false), @EntityOneAction(entityName = "PartyGroup", valueField = "taxAuthPartyGroup", autoFieldMap = false), @EntityOneAction(entityName = "Geo", valueField = "geo", autoFieldMap = false)})
    )
    public interface ListTaxAuthorityParties {}

    @Form(
        name = "EditTaxAuthorityPartyInfo",
        location = "component://accounting/widget/settings/TaxAuthorityForms.xml",
        target = "updateTaxAuthorityPartyInfo",
        defaultMapName = "partyTaxAuthInfo",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", useWhen = "taxAuthority==null", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TaxAuthorityAndGeo", description = "${geoName} [${geoId}]", keyFieldName = "geoId"))),
            @FormField(name = "taxAuthGeoId", title = "${uiLabelMap.CommonGeo}", useWhen = "taxAuthority!=null", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.AccountingTaxAuthorityParty}", useWhen = "taxAuthority==null", position = 2, requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TaxAuthority", description = "${taxAuthPartyId}"))),
            @FormField(name = "taxAuthPartyId", title = "${uiLabelMap.AccountingTaxAuthorityParty}", useWhen = "taxAuthority!=null", position = 2, display = @DisplayField(description = "${taxAuthPartyId}")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyTaxAuthInfo!=null", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName} ${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyTaxAuthInfo==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyTaxId", position = 2, text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "partyTaxAuthInfo!=null", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "partyTaxAuthInfo==null", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "isExempt", widgetStyle = "+smallSelect", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isNexus", widgetStyle = "+smallSelect", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyTaxAuthInfo!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "partyTaxAuthInfo==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "partyTaxAuthInfo==null", target = "createTaxAuthorityPartyInfo")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "TaxAuthority", valueField = "taxAuthority")})
    )
    public interface EditTaxAuthorityPartyInfo {}

}
