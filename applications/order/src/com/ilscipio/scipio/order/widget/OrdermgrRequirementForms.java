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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrRequirementForms {

    @Form(
        name = "FindRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "FindRequirements",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "requirementId", textFind = @TextFindField),
            @FormField(name = "requirementTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RequirementType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "REQUIREMENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityId}"))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "requirementStartDate", title = "${uiLabelMap.OrderRequirementStartDate}", dateFind = @DateFindField(type = "date")),
            @FormField(name = "requiredByDate", title = "${uiLabelMap.OrderRequirementByDate}", dateFind = @DateFindField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindRequirements {}

    @Form(
        name = "ListRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindRequirements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/EditRequirement", urlMode = UrlMode.INTER_APP, description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "requirementTypeId", displayEntity = @DisplayEntityField(entityName = "RequirementType")),
            @FormField(name = "facilityId", display = @DisplayField),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "requirementStartDate", display = @DisplayField),
            @FormField(name = "requiredByDate", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "facilityQuantityOnHandTotal", display = @DisplayField),
            @FormField(name = "quantityOnHandTotal", display = @DisplayField),
            @FormField(name = "requestAction", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ListRequirementCustRequests", description = "${uiLabelMap.OrderRequests}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "ordersAction", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ListRequirementOrders", description = "${uiLabelMap.CommonOrders}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteRequirement", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "requirementId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", value = "Requirement"), @FieldMap(fieldName = "orderBy", value = "statusId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "lookupProductId", value = "${groovy: productId == null? '_NA_' : productId}", type = "String"), @SetAction(field = "lookupFacilityId", value = "${groovy: facilityId == null? '_NA_' : facilityId}", type = "String"), @SetAction(field = "facilityQuantityOnHandTotal", fromField = "resultQoh.quantityOnHandTotal"), @SetAction(field = "quantityOnHandTotal", fromField = "resultQohTotal.quantityOnHandTotal")}, service = {@ServiceAction(serviceName = "getInventoryAvailableByFacility", resultMapName = "resultQoh", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "lookupProductId"), @FieldMap(fieldName = "facilityId", fromField = "lookupFacilityId")}), @ServiceAction(serviceName = "getProductInventoryAvailable", resultMapName = "resultQohTotal", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "lookupProductId")})})
    )
    public interface ListRequirements {}

    @Form(
        name = "EditRequirement",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "updateRequirement",
        defaultMapName = "requirement",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateRequirement", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "requirementId", hidden = @HiddenField),
            @FormField(name = "requirementTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RequirementType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "REQUIREMENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "facilityId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${description} [${facilityId}]"))),
            @FormField(name = "fixedAssetId", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "custRequestId", mapName = "parameters", text = @TextField),
            @FormField(name = "custRequestItemSeqId", mapName = "parameters", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "requirement==null", target = "createRequirement")
        }
    )
    public interface EditRequirement {}

    @Form(
        name = "ListRequirementCustRequests",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        listName = "requirementCustRequests",
        paginateTarget = "ListRequirementCustRequests",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "RequirementCustRequest", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "requirementId", hidden = @HiddenField),
            @FormField(name = "custRequestId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "request", description = "${custRequestId}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "custRequestItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "requestitem", description = "${custRequestItemSeqId}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")}))
        }
    )
    public interface ListRequirementCustRequests {}

    @Form(
        name = "ListRequirementOrders",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        listName = "orderRequirements",
        paginateTarget = "ListRequirementOrders",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderRequirementCommitment", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")}))
        }
    )
    public interface ListRequirementOrders {}

    @Form(
        name = "ListRequirementRoles",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        listName = "requirementRoles",
        paginateTarget = "ListRequirementRoles",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "RequirementRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "requirementId", hidden = @HiddenField),
            @FormField(name = "editAction", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRequirementRole", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "requirementId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeRequirementRole", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "requirementId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListRequirementRoles {}

    @Form(
        name = "EditRequirementRole",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "updateRequirementRole",
        defaultMapName = "requirementRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "requirementId", hidden = @HiddenField),
            @FormField(name = "partyId", useWhen = "requirementRole!=null", display = @DisplayField),
            @FormField(name = "partyId", useWhen = "requirementRole==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", useWhen = "requirementRole!=null", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "roleTypeId", useWhen = "requirementRole==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "requirementRole!=null", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "requirementRole==null", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "requirementRole==null", target = "createRequirementRole")
        }
    )
    public interface EditRequirementRole {}

    @Form(
        name = "FindNotApprovedRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "ApproveRequirements",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "requirementId", textFind = @TextFindField),
            @FormField(name = "requirementTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RequirementType", description = "${description}"))),
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityId}"))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "requirementStartDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "requiredByDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindNotApprovedRequirements {}

    @Form(
        name = "ApproveRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.MULTI,
        target = "approveRequirements",
        listName = "requirements",
        paginateTarget = "ApproveRequirements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRequirement", description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "requirementTypeId", displayEntity = @DisplayEntityField(entityName = "RequirementType")),
            @FormField(name = "facilityId", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "requirementStartDate", display = @DisplayField),
            @FormField(name = "requiredByDate", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ApproveRequirements {}

    @Form(
        name = "FindApprovedProductRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "ApprovedProductRequirements",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "showList", hidden = @HiddenField(value = "Y")),
            @FormField(name = "requirementId", textFind = @TextFindField),
            @FormField(name = "billToCustomerPartyId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "SUPPLIER")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "supplierCurrencyUomId", entryName = "parameters.supplierCurrencyUomId", position = 2, display = @DisplayField),
            @FormField(name = "unassignedRequirements", check = @CheckField),
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityId}"))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "requirementByDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindApprovedProductRequirements {}

    @Form(
        name = "ApprovedProductRequirementsList",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        listName = "requirementsForSupplier",
        paginateTarget = "RequirementsForSupplier",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRequirement", description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductInventoryItems", urlMode = UrlMode.INTER_APP, description = "${productId}", targetWindow = "top", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "internalName", entryName = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "facilityId", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${LastName} ${firstName} ${middleName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile?partyId=${partyId}", description = "[${partyId}]", linkStyle = "${styles.link_nav_info_id}"))),
            @FormField(name = "supplierProductId", title = "${uiLabelMap.ProductSupplierProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductSuppliers?productId=${productId}", urlMode = UrlMode.INTER_APP, description = "${supplierProductId}")),
            @FormField(name = "idValue", title = "${uiLabelMap.ProductUPCA}", display = @DisplayField),
            @FormField(name = "minimumOrderQuantity", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "lastPrice", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "requiredByDate", display = @DisplayField),
            @FormField(name = "quantity", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "comments", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "prepareFind", resultMapName = "resultConditions", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", value = "Requirement")}), @ServiceAction(serviceName = "getRequirementsForSupplier", resultMapName = "result", resultMapList = "requirementsForSupplier", fieldMaps = {@FieldMap(fieldName = "requirementConditions", fromField = "resultConditions.entityConditionList"), @FieldMap(fieldName = "partyId", fromField = "parameters.partyId"), @FieldMap(fieldName = "unassignedRequirements", fromField = "parameters.unassignedRequirements")})})
    )
    public interface ApprovedProductRequirementsList {}

    @Form(
        name = "ApprovedProductRequirements",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.MULTI,
        target = "quickPurchaseOrderEntry",
        listName = "requirementsForSupplier",
        paginateTarget = "RequirementsForSupplier",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "orderId", idName = "orderId", hidden = @HiddenField),
            @FormField(name = "billToCustomerPartyId", hidden = @HiddenField(value = "${parameters.billToCustomerPartyId}")),
            @FormField(name = "supplierPartyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRequirement", description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductInventoryItems", urlMode = UrlMode.INTER_APP, description = "${productId}", targetWindow = "top", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "facilityId", hidden = @HiddenField(value = "${parameters.facilityId}")),
            @FormField(name = "internalName", entryName = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${lastName} ${firstName} ${groupName}")),
            @FormField(name = "supplierCurrencyUomId", entryName = "parameters.supplierCurrencyUomId", display = @DisplayField),
            @FormField(name = "supplierProductId", title = "${uiLabelMap.ProductSupplierProductId}", display = @DisplayField),
            @FormField(name = "idValue", title = "${uiLabelMap.ProductUPCA}", display = @DisplayField),
            @FormField(name = "minimumOrderQuantity", title = "${uiLabelMap.FormFieldTitle_minimumOrderQuantity}", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "lastPrice", widgetAreaStyle = "aount", display = @DisplayField(type = "currency")),
            @FormField(name = "requiredByDate", display = @DisplayField),
            @FormField(name = "atp", title = "${uiLabelMap.ProductAtp}", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "qoh", title = "${uiLabelMap.ProductQoh}", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "qtySold", title = "${uiLabelMap.OrderQuantitySold}", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "quantity", text = @TextField(size = 4)),
            @FormField(name = "comments", display = @DisplayField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField)
        }
    )
    public interface ApprovedProductRequirements {}

    @Form(
        name = "ApprovedProductRequirementsSubmit",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "orderId", idName = "orderId_o_0", text = @TextField),
            @FormField(name = "submitAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "javascript:document.ApprovedProductRequirements.orderId_o_0.value=document.ApprovedProductRequirementsSubmit.orderId_o_0.value;document.ApprovedProductRequirements.submit()", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.OrderInputQuickPurchaseOrder}", alsoHidden = false))
        }
    )
    public interface ApprovedProductRequirementsSubmit {}

    @Form(
        name = "ApprovedProductRequirementsSummary",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        target = "ApprovedProductRequirements",
        defaultMapName = "quantityReport",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "distinctProductCount", title = "${uiLabelMap.OrderRequirementNumberOfProducts}", display = @DisplayField),
            @FormField(name = "quantityTotal", display = @DisplayField),
            @FormField(name = "amountTotal", display = @DisplayField(type = "currency"))
        }
    )
    public interface ApprovedProductRequirementsSummary {}

    @Form(
        name = "ApprovedProductRequirementsByVendor",
        location = "component://order/widget/ordermgr/RequirementForms.xml",
        type = FormType.LIST,
        target = "ApprovedProductRequirements",
        listName = "requirements",
        paginateTarget = "ApprovedProductRequirementsByVendor",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "partyId", display = @DisplayField(description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, partyId, false);}")),
            @FormField(name = "supplierCurrencyUomId", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.OrderVendorRequirementCount}", widgetAreaStyle = "align-text", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "billToCustomerPartyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${firstName} ${lastName} ${groupName} (${partyId})", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "firstName"), @EntityOrderBy(fieldName = "lastName"), @EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "prepareFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "facilityId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} (${facilityId})", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "prepareAction", title = "${uiLabelMap.OrderPrepareOrder}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "supplierCurrencyUomId", fromField = "supplierParty.preferredCurrencyUomId")}, entityOne = {@EntityOneAction(entityName = "Party", valueField = "supplierParty")})
    )
    public interface ApprovedProductRequirementsByVendor {}

}
