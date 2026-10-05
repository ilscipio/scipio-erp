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
public class CatalogStoreForms {

    @Form(
        name = "ListProductStore",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        listName = "productStores",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", widgetStyle = "${styles.link_nav_info_idname}", sortField = true, hyperlink = @HyperlinkField(target = "EditProductStore", description = "${storeName} [${productStoreId}]", parameters = {@ParameterDef(paramName = "productStoreId")})),
            @FormField(name = "companyName", title = "${uiLabelMap.CommonCompany}", sortField = true, display = @DisplayField),
            @FormField(name = "title", title = "${uiLabelMap.ProductTitle}", sortField = true, display = @DisplayField),
            @FormField(name = "inventoryFacilityId", sortField = true, displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName}")),
            @FormField(name = "defaultPriority", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "storeName")})
    )
    public interface ListProductStore {}

    @Form(
        name = "FindProductStoreRole",
        location = "component://product/widget/catalog/StoreForms.xml",
        target = "FindProductStoreRoles",
        defaultEntityName = "ProductStoreRole",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindProductStoreRole {}

    @Form(
        name = "ListProductStoreRole",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "ProductStoreRole",
        paginateTarget = "FindProductStoreRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName},${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "sequenceNum", display = @DisplayField),
            @FormField(name = "editAction", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "FindProductStoreRoles", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "storeRemoveRole", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductStoreRole"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListProductStoreRole {}

    @Form(
        name = "EditProductStoreRole",
        location = "component://product/widget/catalog/StoreForms.xml",
        target = "storeUpdateRole",
        defaultMapName = "productStoreRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "productStoreRole==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "productStoreRole!=null", display = @DisplayField),
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", useWhen = "productStoreRole==null", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", useWhen = "productStoreRole!=null", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productStoreRole==null", dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productStoreRole!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "productStoreRole==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", useWhen = "productStoreRole!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStoreRole==null", target = "storeCreateRole")
        }
    )
    public interface EditProductStoreRole {}

    @Form(
        name = "ListProductStoreFacility",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "facilityId", displayEntity = @DisplayEntityField(entityName = "Facility", description = "${facilityName}", subHyperlink = @SubHyperlink(target = "/facility/control/EditFacility", description = "[${facilityId}]", linkStyle = "link", parameters = {@ParameterDef(paramName = "facilityId")}))),
            @FormField(name = "sequenceNum", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date-time")),
            @FormField(name = "submitAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "ProductStoreFacilities", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "productStoreId", fromField = "productStoreId"), @ParameterDef(paramName = "facilityId", fromField = "facilityId"), @ParameterDef(paramName = "fromDate", fromField = "fromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductStoreFacility", description = "${uiLabelMap.CommonRemove}", linkType = "hidden-form", requestConfirmation = "true", parameters = {@ParameterDef(paramName = "productStoreId", fromField = "productStoreId"), @ParameterDef(paramName = "facilityId", fromField = "facilityId"), @ParameterDef(paramName = "fromDate", fromField = "fromDate")}))
        }
    )
    public interface ListProductStoreFacility {}

    @Form(
        name = "EditProductStoreFacility",
        location = "component://product/widget/catalog/StoreForms.xml",
        target = "addProductStoreFacility",
        focusFieldName = "facilityId",
        defaultMapName = "productStoreFacility",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "facilityId", useWhen = "productStoreFacility != null", display = @DisplayField),
            @FormField(name = "facilityId", useWhen = "productStoreFacility == null", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "sequenceNum", text = @TextField(size = 3)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productStoreFacility != null", requiredField = true, display = @DisplayField(type = "date-time")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productStoreFacility == null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${groovy: productStoreFacility == null ? uiLabelMap.CommonAdd : uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStoreFacility != null", target = "updateProductStoreFacility")
        },
        actions = @FormActions(set = {@SetAction(field = "useRequestParameters", value = "false", type = "Boolean")})
    )
    public interface EditProductStoreFacility {}

    @Form(
        name = "ListParentProductStoreGroup",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        target = "EditProductStoreGroup",
        paginate = "false",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreGroupId", hidden = @HiddenField),
            @FormField(name = "productStoreGroupName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditProductStoreGroup", description = "${productStoreGroupName} [${productStoreGroupId}]", parameters = {@ParameterDef(paramName = "productStoreGroupId")})),
            @FormField(name = "productStoreGroupTypeId", displayEntity = @DisplayEntityField(entityName = "ProductStoreGroupType")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "primaryParentGroupId", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditProductStoreGroup", description = "${primaryParentGroupId}", parameters = {@ParameterDef(paramName = "primaryParentGroupId")}))
        }
    )
    public interface ListParentProductStoreGroup {}

    @Form(
        name = "EditProductStoreGroup",
        location = "component://product/widget/catalog/StoreForms.xml",
        target = "updateProductStoreGroup",
        defaultMapName = "productStoreGroup",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductStoreGroup")
        },
        fields = {
            @FormField(name = "productStoreGroupId", hidden = @HiddenField),
            @FormField(name = "productStoreGroupTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStoreGroupType", description = "${description}"))),
            @FormField(name = "primaryParentGroupId", useWhen = "productStoreGroup == null", ignored = @IgnoredField),
            @FormField(name = "primaryParentGroupId", useWhen = "productStoreGroup != null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStoreGroup", description = "${productStoreGroupName} [${productStoreGroupId}]", keyFieldName = "productStoreGroupId", constraints = {@EntityConstraint(name = "productStoreGroupId", envName = "productStoreGroup.productStoreGroupId", operator = "not-equals")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.Common${groovy: context.productStoreGroup?'Submit':'Create'}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStoreGroup == null", target = "createProductStoreGroup")
        }
    )
    public interface EditProductStoreGroup {}

    @Form(
        name = "ListProductStoreGroupAssoc",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreGroupId", displayEntity = @DisplayEntityField(entityName = "ProductStoreGroup", description = "${productStoreGroupName}", subHyperlink = @SubHyperlink(target = "EditProductStoreGroupAndAssoc", description = " [${productStoreGroupId}]", parameters = {@ParameterDef(paramName = "productStoreId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "submitAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", hyperlink = @HyperlinkField(target = "updateProductStoreGroupRollup", description = "${uiLabelMap.CommonDelete}", requestConfirmation = "true", parameters = {@ParameterDef(paramName = "productStoreGroupId"), @ParameterDef(paramName = "parentGroupId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", fromField = "date:nowTimestamp()")}))
        }
    )
    public interface ListProductStoreGroupAssoc {}

    @Form(
        name = "ListProductStoreAssoc",
        location = "component://product/widget/catalog/StoreForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductStore", description = "${productStoreId}", parameters = {@ParameterDef(paramName = "productStoreId")})),
            @FormField(name = "storeName", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        }
    )
    public interface ListProductStoreAssoc {}

    @Form(
        name = "AddProductStoreAssoc",
        location = "component://product/widget/catalog/StoreForms.xml",
        target = "AddProductStoreToGroup",
        fields = {
            @FormField(name = "productStoreGroupId", hidden = @HiddenField),
            @FormField(name = "productStoreId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}", type = "date")),
            @FormField(name = "addAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "submit", areaId = "centerdiv", areaTarget = "EditProductStoreGroupAndAssoc")
        }
    )
    public interface AddProductStoreAssoc {}

}
