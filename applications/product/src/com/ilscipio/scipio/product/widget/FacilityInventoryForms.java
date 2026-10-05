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
public class FacilityInventoryForms {

    @Form(
        name = "EditInventoryItem",
        location = "component://product/widget/facility/InventoryForms.xml",
        target = "UpdateInventoryItem",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateInventoryItem", mapName = "inventoryItem")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "inventoryItem==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "oldAvailableToPromise", ignored = @IgnoredField),
            @FormField(name = "oldQuantityOnHand", ignored = @IgnoredField),
            @FormField(name = "inventoryItemId", tooltip = "${uiLabelMap.ProductNotModificationRecrationInventoryItem}", useWhen = "inventoryItem!=null", display = @DisplayField),
            @FormField(name = "inventoryItemId", useWhen = "inventoryItem==null", ignored = @IgnoredField),
            @FormField(name = "inventoryItemTypeId", title = "${uiLabelMap.ProductInventoryItemTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InventoryItemType", description = "${description}", keyFieldName = "inventoryItemTypeId"))),
            @FormField(name = "productId", useWhen = "productId!=null", requiredField = true, lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productId", useWhen = "productId==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "inventoryItem==null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "INV_NON_SER_STTS")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "inventoryItem!=null&&\"SERIALIZED_INV_ITEM\".equals(inventoryItem.getString(\"inventoryItemTypeId\"))", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "INV_SERIALIZED_STTS")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "inventoryItem!=null&&\"NON_SERIAL_INV_ITEM\".equals(inventoryItem.getString(\"inventoryItemTypeId\"))", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "INV_NON_SER_STTS")}))),
            @FormField(name = "expireDate", title = "${uiLabelMap.ProductExpireDate}"),
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}"),
            @FormField(name = "uomId", title = "${uiLabelMap.ProductUomId}"),
            @FormField(name = "binNumber", title = "${uiLabelMap.ProductBinNumber}"),
            @FormField(name = "locationSeqId", title = "${uiLabelMap.ProductFacilityLocation}", lookup = @LookupField(targetFormName = "LookupFacilityLocation")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "ownerPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "unitCost", text = @TextField),
            @FormField(name = "accountingQuantityTotal", useWhen = "inventoryItem!=null", display = @DisplayField),
            @FormField(name = "accountingQuantityTotal", useWhen = "inventoryItem==null", ignored = @IgnoredField),
            @FormField(name = "totals", title = "${uiLabelMap.ProductAvailablePromiseQuantityHand}", useWhen = "inventoryItem!=null", display = @DisplayField(description = "${inventoryItem.availableToPromiseTotal} / ${inventoryItem.quantityOnHandTotal}")),
            @FormField(name = "submit", title = "${uiLabelMap.CommonUpdate}", useWhen = "inventoryItem!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonCreate}", useWhen = "inventoryItem==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "inventoryItem==null", target = "CreateInventoryItem")
        },
        actions = @FormActions(set = {@SetAction(field = "inventoryItemTypeId", fromField = "inventoryItem.inventoryItemTypeId"), @SetAction(field = "productId", fromField = "inventoryItem.productId"), @SetAction(field = "facilityId", fromField = "inventoryItem.facilityId"), @SetAction(field = "locationSeqId", fromField = "inventoryItem.locationSeqId"), @SetAction(field = "statusId", fromField = "inventoryItem.statusId")}, entityOne = {@EntityOneAction(entityName = "FacilityLocation", valueField = "facilityLocation")}),
        sortOrder = @SortOrder(sortFields = {@SortField(name = "inventoryItemId"), @SortField(name = "inventoryItemTypeId"), @SortField(name = "productId"), @SortField(name = "totals"), @SortField(name = "accountingQuantityTotal"), @SortField(name = "partyId"), @SortField(name = "ownerPartyId"), @SortField(name = "statusId"), @SortField(name = "datetimeReceived"), @SortField(name = "datetimeManufactured"), @SortField(name = "expireDate"), @SortField(name = "facilityId"), @SortField(name = "containerId"), @SortField(name = "lotId"), @SortField(name = "uomId"), @SortField(name = "binNumber"), @SortField(name = "locationSeqId"), @SortField(name = "comments"), @SortField(name = "serialNumber"), @SortField(name = "softIdentifier"), @SortField(name = "activationNumber"), @SortField(name = "activationValidThru"), @SortField(name = "unitCost"), @SortField(name = "currencyUomId"), @SortField(name = "fixedAssetId"), @SortField(name = "submit")})
    )
    public interface EditInventoryItem {}

    @Form(
        name = "CreatePhysicalInventoryAndVariance",
        location = "component://product/widget/facility/InventoryForms.xml",
        target = "createPhysicalInventoryAndVariance",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPhysicalInventoryAndVariance")
        },
        fields = {
            @FormField(name = "physicalInventoryId", ignored = @IgnoredField),
            @FormField(name = "physicalInventoryDate", ignored = @IgnoredField),
            @FormField(name = "partyId", ignored = @IgnoredField),
            @FormField(name = "generalComments", ignored = @IgnoredField),
            @FormField(name = "inventoryItemId", mapName = "inventoryItem", hidden = @HiddenField),
            @FormField(name = "varianceReasonId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "VarianceReason", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "comments"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreatePhysicalInventoryAndVariance {}

    @Form(
        name = "ViewPhysicalInventoryAndVariance",
        location = "component://product/widget/facility/InventoryForms.xml",
        type = FormType.LIST,
        listName = "physicalInventoryAndVarianceDatas",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PhysicalInventoryAndVariance", mapName = "physicalInventoryAndVariance", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "inventoryItemId", hidden = @HiddenField),
            @FormField(name = "partyId", display = @DisplayField(description = "${person.firstName} ${person.lastName} ${partyGroup.groupName} [${physicalInventoryAndVariance.partyId}]")),
            @FormField(name = "varianceReasonId", display = @DisplayField(description = "${varianceReason.description}"))
        }
    )
    public interface ViewPhysicalInventoryAndVariance {}

    @Form(
        name = "ViewInventoryItemShipmentReceipts",
        location = "component://product/widget/facility/InventoryForms.xml",
        type = FormType.LIST,
        listName = "shipmentReceiptList",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentReceipt", mapName = "shipmentReceipt", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "inventoryItemId", hidden = @HiddenField)
        }
    )
    public interface ViewInventoryItemShipmentReceipts {}

    @Form(
        name = "ListInventoryItemDetail",
        location = "component://product/widget/facility/InventoryForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemDetails",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "InventoryItemDetail", mapName = "inventoryItemDetail", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "inventoryItemId", hidden = @HiddenField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "reasonEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}"))
        }
    )
    public interface ListInventoryItemDetail {}

    @Form(
        name = "InventoryItemReservations",
        location = "component://product/widget/facility/InventoryForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemReservations",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderItemShipGrpInvRes", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "inventoryItemId", hidden = @HiddenField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")}))
        }
    )
    public interface InventoryItemReservations {}

    @Form(
        name = "UpdateInventoryItemLabelAppls",
        location = "component://product/widget/facility/InventoryForms.xml",
        type = FormType.LIST,
        target = "updateInventoryItemLabelApplFromItem",
        listName = "inventoryItemLabelAppls",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateInventoryItemLabelAppl")
        },
        fields = {
            @FormField(name = "inventoryItemLabelId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItemLabel", description = "${inventoryItemLabelId}", parameters = {@ParameterDef(paramName = "inventoryItemLabelId")})),
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "inventoryItemId", hidden = @HiddenField),
            @FormField(name = "inventoryItemLabelTypeId", displayEntity = @DisplayEntityField(entityName = "InventoryItemLabelType", description = "${description} [${inventoryItemLabelTypeId}]")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteInventoryItemLabelApplFromItem", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "inventoryItemLabelTypeId"), @ParameterDef(paramName = "inventoryItemLabelId"), @ParameterDef(paramName = "facilityId")}))
        }
    )
    public interface UpdateInventoryItemLabelAppls {}

    @Form(
        name = "AddInventoryItemLabelAppl",
        location = "component://product/widget/facility/InventoryForms.xml",
        target = "createInventoryItemLabelApplFromItem",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createInventoryItemLabelAppl")
        },
        fields = {
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "inventoryItemId", hidden = @HiddenField),
            @FormField(name = "inventoryItemLabelId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InventoryItemLabel", description = "${inventoryItemLabelTypeId} ${inventoryItemLabelId} ${description}", orderBy = {@EntityOrderBy(fieldName = "inventoryItemLabelTypeId"), @EntityOrderBy(fieldName = "inventoryItemLabelId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddInventoryItemLabelAppl {}

}
