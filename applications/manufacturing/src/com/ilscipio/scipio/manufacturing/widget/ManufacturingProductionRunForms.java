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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingProductionRunForms {

    @Form(
        name = "CreateProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "createProductionRun",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProduct", size = 16)),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingQuantity}", requiredField = true, tooltip = "${uiLabelMap.ManufacturingProductionRunQuantityTooltip}", text = @TextField(size = 6)),
            @FormField(name = "startDate", title = "${uiLabelMap.ManufacturingStartDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "routingId", title = "${uiLabelMap.ManufacturingRoutingId}", tooltip = "${uiLabelMap.ManufacturingRoutingIdTooltip}", lookup = @LookupField(targetFormName = "LookupRouting", size = 16)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", text = @TextField(size = 30)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 50)),
            @FormField(name = "createDependentProductionRuns", title = "${uiLabelMap.ManufacturingCreateDependentProductionRuns}", tooltip = "${uiLabelMap.ManufacturingCreateDependentProductionRunsTooltip}", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductionRun {}

    @Form(
        name = "findProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "FindProductionRun",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "PROD_ORDER_HEADER")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingProductionRunId}", textFind = @TextFindField),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PRODUCTION_RUN")}))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", textFind = @TextFindField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", dateFind = @DateFindField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findProductionRun {}

    @Form(
        name = "listFindProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindProductionRun",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ShowProductionRun", description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productionRunId", fromField = "workEffortId")})),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField),
            @FormField(name = "QuantityUom", title = "${uiLabelMap.ProductQuantityUom}", display = @DisplayField(description = "${uom.abbreviation}")),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", value = "WorkEffortAndGoods"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "orderBy", value = "estimatedStartDate")})}),
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product"), @EntityOneAction(entityName = "Uom", valueField = "uom")})
    )
    public interface listFindProductionRun {}

    @Form(
        name = "UpdateProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "updateProductionRun",
        defaultMapName = "productionRunData",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${internalName} [${productId}]")),
            @FormField(name = "currentStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "fabricationOrderId", title = "${uiLabelMap.ManufacturingFabricationOrder}", useWhen = "productionRun != null && productionRun.workEffortParentId != null", hyperlink = @HyperlinkField(target = "EditFabricationOrder", description = "${productionRun.workEffortParentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fabricationOrderId", value = "${productionRun.workEffortParentId}")})),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", constraints = {@EntityConstraint(name = "facilityTypeId", value = "WAREHOUSE")}))),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingQuantity}", text = @TextField(size = 10)),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", dateTime = @DateTimeField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface UpdateProductionRun {}

    @Form(
        name = "ListProductionRunOrderItems",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "EditProductionRun",
        listName = "orderItems",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}/${orderItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId")}))
        }
    )
    public interface ListProductionRunOrderItems {}

    @Form(
        name = "ListProductionRunInventoryItems",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "inventoryItems",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditInventoryItem", urlMode = UrlMode.INTER_APP, description = "${inventoryItemId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "inventoryItemId")})),
            @FormField(name = "lotId", mapName = "inventoryItem", display = @DisplayField),
            @FormField(name = "statusId", mapName = "inventoryItem", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "unitCost", mapName = "inventoryItem", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "quantity", entryName = "quantityOnHandDiff", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "datetimeReceived", mapName = "inventoryItem", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "quantityOnHandDiff", fromField = "inventoryItemDetails[0].quantityOnHandDiff")}, entityOne = {@EntityOneAction(entityName = "InventoryItem", valueField = "inventoryItem")})
    )
    public interface ListProductionRunInventoryItems {}

    @Form(
        name = "ViewListProductionRunRoutingTasks",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "productionRunRoutingTasks",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "priority", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingMachine}", display = @DisplayField),
            @FormField(name = "reservPersons", display = @DisplayField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "estimatedTotalMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedTotalMilliSeconds}", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "estimatedTotalMilliSeconds", value = "${groovy: (estimatedMilliSeconds) ? estimatedMilliSeconds * quantity : 0}", type = "BigDecimal")})
    )
    public interface ViewListProductionRunRoutingTasks {}

    @Form(
        name = "ListProductionRunRoutingTasks",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "ProductionRunTasks",
        listName = "productionRunRoutingTasks",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "priority", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingMachine}", display = @DisplayField),
            @FormField(name = "reservPersons", display = @DisplayField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "estimatedTotalMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedTotalMilliSeconds}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "ProductionRunTasks", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "routingTaskId", fromField = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductionRunRoutingTask", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "estimatedTotalMilliSeconds", value = "${groovy: (estimatedMilliSeconds) ? estimatedMilliSeconds * quantity : 0}", type = "BigDecimal")})
    )
    public interface ListProductionRunRoutingTasks {}

    @Form(
        name = "ListProductionRunComponents",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "EditProductionRun",
        listName = "productionRunComponents",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductName}", display = @DisplayField(description = "${product.internalName} [${productId}]")),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField),
            @FormField(name = "quantityUomId", mapName = "product", title = "${uiLabelMap.CommonUom}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${abbreviation}"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product", useCache = true)})
    )
    public interface ListProductionRunComponents {}

    @Form(
        name = "EditProductionRunRoutingTask",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "updateProductionRunRoutingTask",
        defaultMapName = "productionRunTask",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField(value = "${productionRunId}")),
            @FormField(name = "routingTaskId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", useWhen = "productionRunTask==null", lookup = @LookupField(targetFormName = "LookupRoutingTask")),
            @FormField(name = "routingTaskId", mapName = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", useWhen = "productionRunTask!=null", display = @DisplayField),
            @FormField(name = "priority", title = "${uiLabelMap.CommonSequenceNum}", text = @TextField(size = 4)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "reservPersons", text = @TextField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", dateTime = @DateTimeField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", text = @TextField),
            @FormField(name = "estimatedMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productionRunTask==null", target = "addProductionRunRoutingTask")
        }
    )
    public interface EditProductionRunRoutingTask {}

    @Form(
        name = "ShowProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "productionRunProduce",
        defaultMapName = "productionRunData",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${internalName} [${productId}]")),
            @FormField(name = "currentStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantityToProduce}", display = @DisplayField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingEstimatedStartDate}", display = @DisplayField),
            @FormField(name = "actualStartDate", title = "${uiLabelMap.CommonActualStartDate}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "actualCompletionDate", title = "${uiLabelMap.ManufacturingActualCompletionDate}", display = @DisplayField),
            @FormField(name = "productionRunName", title = "${uiLabelMap.ManufacturingProductionRunName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.FacilityFacility}", displayEntity = @DisplayEntityField(entityName = "Facility", description = "${facilityName} [${facilityId}]")),
            @FormField(name = "quantityProduced", title = "${uiLabelMap.ManufacturingQuantityProduced}", display = @DisplayField),
            @FormField(name = "quantityRejected", title = "${uiLabelMap.ManufacturingQuantityRejected}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "useRequestParameters", value = "false", type = "Boolean")})
    )
    public interface ShowProductionRun {}

    @Form(
        name = "ProductionRunProduce",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "productionRunProduce",
        defaultMapName = "productionRunData",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingProduceQuantity}", text = @TextField),
            @FormField(name = "inventoryItemTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InventoryItemType", description = "${description}"))),
            @FormField(name = "lotId", text = @TextField(defaultValue = "${lastLotId}")),
            @FormField(name = "locationSeqId", title = "${uiLabelMap.ProductLocationSeqId}", tooltip = "${uiLabelMap.ManufacturingProduceLocationSeqIdTooltip}", lookup = @LookupField(targetFormName = "LookupFacilityLocation", targetParameter = "facilityId")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface ProductionRunProduce {}

    @Form(
        name = "ProductionRunDeclareAndProduceTop",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "productionRunDeclareAndProduce",
        defaultMapName = "productionRunData",
        headerRowStyle = "header-row",
        skipEnd = "true",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingProduceQuantity}", tooltip = "${uiLabelMap.ManufacturingProduceQuantityMessage}", text = @TextField),
            @FormField(name = "inventoryItemTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InventoryItemType", description = "${description}"))),
            @FormField(name = "lotId", text = @TextField(defaultValue = "${lastLotId}"))
        }
    )
    public interface ProductionRunDeclareAndProduceTop {}

    @Form(
        name = "ProductionRunDeclareAndProduceBottom",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.MULTI,
        listName = "productionRunComponentsDataReadyForIssuance",
        oddRowStyle = "alternate-row",
        skipStart = "true",
        fields = {
            @FormField(name = "productionRunTaskId", entryName = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "failIfItemsAreNotAvailable", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "task", entryName = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]", alsoHidden = false)),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField(description = "${internalName} [${productId}]", alsoHidden = false)),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "issuedQuantity", title = "${uiLabelMap.ManufacturingIssuedQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "locationSeqId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFacilityLocation", description = "${locationSeqId}", constraints = {@EntityConstraint(name = "productId", envName = "productId"), @EntityConstraint(name = "facilityId", envName = "facilityId")}))),
            @FormField(name = "secondaryLocationSeqId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFacilityLocation", description = "${locationSeqId}", keyFieldName = "locationSeqId", constraints = {@EntityConstraint(name = "productId", envName = "productId"), @EntityConstraint(name = "facilityId", envName = "facilityId")}))),
            @FormField(name = "_rowSubmit", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface ProductionRunDeclareAndProduceBottom {}

    @Form(
        name = "ListProductionRunDeclRoutingTasks",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "ProductionRunDeclaration",
        listName = "productionRunRoutingTasks",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "actionForm", hidden = @HiddenField(value = "EditRoutingTask")),
            @FormField(name = "productionRunId", hidden = @HiddenField(value = "${workEffortParentId}")),
            @FormField(name = "routingTaskId", hidden = @HiddenField(value = "${workEffortId}")),
            @FormField(name = "priority", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingMachine}", display = @DisplayField),
            @FormField(name = "reservPersons", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "actualSetupMillis", title = "${uiLabelMap.ManufacturingTaskActualSetupMillis}", display = @DisplayField),
            @FormField(name = "actualMilliSeconds", title = "${uiLabelMap.ManufacturingTaskActualMilliSeconds}", display = @DisplayField),
            @FormField(name = "quantityProduced", title = "${uiLabelMap.ManufacturingQuantityProduced}", display = @DisplayField),
            @FormField(name = "startAction", title = " ", useWhen = "\"${startTaskId}\".equals(\"${workEffortId}\")", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", hyperlink = @HyperlinkField(target = "changeProductionRunTaskStatus", description = "${uiLabelMap.ManufacturingStartProductionRunTask}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId")})),
            @FormField(name = "issueLinkAtp", title = " ", useWhen = "\"${issueTaskId}\".equals(\"${workEffortId}\")", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "issueProductionRunRoutingTask", description = "${uiLabelMap.ManufacturingIssueAvailableProductionRunTask}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId"), @ParameterDef(paramName = "failIfItemsAreNotAvailable", value = "Y")})),
            @FormField(name = "issueLinkQoh", title = " ", useWhen = "\"${issueTaskId}\".equals(\"${workEffortId}\")", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "issueProductionRunRoutingTask", description = "${uiLabelMap.ManufacturingIssueProductionRunTask}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId"), @ParameterDef(paramName = "failIfItemsAreNotAvailable", value = "N")})),
            @FormField(name = "declareAction", title = " ", useWhen = "\"PRUN_RUNNING\".equals(\"${currentStatusId}\")", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ProductionRunDeclaration", description = "${uiLabelMap.ManufacturingDeclareProductionRunTask}", alsoHidden = false, parameters = {@ParameterDef(paramName = "actionForm", value = "EditRoutingTask"), @ParameterDef(paramName = "routingTaskId", fromField = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId")})),
            @FormField(name = "completeAction", title = " ", useWhen = "\"${completeTaskId}\".equals(\"${workEffortId}\")", widgetStyle = "${styles.link_run_sys} ${styles.action_complete}", hyperlink = @HyperlinkField(target = "changeProductionRunTaskStatus", description = "${uiLabelMap.ManufacturingCompleteProductionRunTask}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productionRunId", fromField = "workEffortParentId")}))
        }
    )
    public interface ListProductionRunDeclRoutingTasks {}

    @Form(
        name = "ListIssueProductionRunDeclComponents",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.MULTI,
        target = "issueProductionRunTaskComponents?productionRunId=${productionRunId}",
        listName = "productionRunComponentsDataReadyForIssuance",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "failIfItemsAreNotAvailable", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "task", entryName = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField(description = "${internalName} [${productId}]", alsoHidden = false)),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "issuedQuantity", title = "${uiLabelMap.ManufacturingIssuedQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "locationSeqId", tooltip = "${uiLabelMap.ManufacturingIssueLocationSeqIdTooltip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFacilityLocation", description = "${locationSeqId}", constraints = {@EntityConstraint(name = "productId", envName = "productId"), @EntityConstraint(name = "facilityId", envName = "facilityId")}))),
            @FormField(name = "secondaryLocationSeqId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFacilityLocation", description = "${locationSeqId}", keyFieldName = "locationSeqId", constraints = {@EntityConstraint(name = "productId", envName = "productId"), @EntityConstraint(name = "facilityId", envName = "facilityId")}))),
            @FormField(name = "lotId", tooltip = "${uiLabelMap.ManufacturingIssueLotIdTooltip}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListIssueProductionRunDeclComponents {}

    @Form(
        name = "ListReturnProductionRunDeclComponents",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.MULTI,
        target = "productionRunTaskReturnMaterials?productionRunId=${productionRunId}",
        listName = "productionRunComponentsAlreadyIssued",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "task", entryName = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", display = @DisplayField(description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField(description = "${internalName} [${productId}]", alsoHidden = false)),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "issuedQuantity", title = "${uiLabelMap.ManufacturingIssuedQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "returnedQuantity", title = "${uiLabelMap.ManufacturingReturnedQuantity}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", text = @TextField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelected}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListReturnProductionRunDeclComponents {}

    @Form(
        name = "EditProductionRunDeclRoutingTask",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "updateProductionRunTaskDeclaration",
        defaultMapName = "routingTaskData",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "actionForm", hidden = @HiddenField(value = "${actionForm}")),
            @FormField(name = "productionRunId", hidden = @HiddenField(value = "${routingTaskData.workEffortParentId}")),
            @FormField(name = "productionRunTaskId", hidden = @HiddenField(value = "${routingTaskData.workEffortId}")),
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${routingTaskData.workEffortId}")),
            @FormField(name = "priority", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.ManufacturingFromDate}", dateTime = @DateTimeField),
            @FormField(name = "toDate", title = "${uiLabelMap.ManufacturingToDate}", dateTime = @DateTimeField),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "actualSetupMillis", title = "${uiLabelMap.ManufacturingTaskActualSetupMillis}", display = @DisplayField),
            @FormField(name = "addSetupTime", title = "${uiLabelMap.ManufacturingAddSetupTime}", parameterName = "setupMinutes", text = @TextField),
            @FormField(name = "estimatedMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", display = @DisplayField),
            @FormField(name = "actualMilliSeconds", title = "${uiLabelMap.ManufacturingTaskActualMilliSeconds}", display = @DisplayField),
            @FormField(name = "addTaskTime", title = "${uiLabelMap.ManufacturingAddTaskTime}", parameterName = "taskMinutes", text = @TextField),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantityToProduce}", display = @DisplayField),
            @FormField(name = "quantityProduced", title = "${uiLabelMap.ManufacturingQuantityProduced}", display = @DisplayField),
            @FormField(name = "addQuantityProduced", title = "${uiLabelMap.ManufacturingAddQuantityProduced}", parameterName = "quantityProduced", text = @TextField),
            @FormField(name = "quantityRejected", title = "${uiLabelMap.ManufacturingQuantityRejected}", display = @DisplayField),
            @FormField(name = "addQuantityRejected", title = "${uiLabelMap.ManufacturingAddQuantityRejected}", parameterName = "quantityRejected", text = @TextField),
            @FormField(name = "reasonEnumId", title = "${uiLabelMap.ManufacturingRejectReason}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}", constraints = {@EntityConstraint(name = "enumTypeId", value = "PRUN_REJECT_REASON")}))),
            @FormField(name = "issueRequiredComponents", title = "${uiLabelMap.ManufacturingIssueComponentsBackflush}", check = @CheckField),
            @FormField(name = "comments", title = "${uiLabelMap.ManufacturingComments}", text = @TextField),
            @FormField(name = "partyId", title = "${uiLabelMap.ManufacturingWorker}", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "EMPLOYEE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/generated/EditProductionRunDeclRoutingTask_script1.groovy")})
    )
    public interface EditProductionRunDeclRoutingTask {}

    @Form(
        name = "CreateRoutingTaskDelivProduct",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "createProductionRunTaskProduct",
        defaultMapName = "formData",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", constraints = {@EntityConstraint(name = "workEffortParentId", envName = "productionRunId")}, orderBy = {@EntityOrderBy(fieldName = "workEffortId")}))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", useWhen = "${groovy:delivProducts.size()>0}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "delivProducts", keyName = "productId", description = "${productId}"))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", useWhen = "${groovy:delivProducts.size()==0}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingAddQuantityProduced}", text = @TextField),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateRoutingTaskDelivProduct {}

    @Form(
        name = "ProductionRunTaskInventoryProducedList",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "prunInventoryProduced",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", title = "${uiLabelMap.ProductInventoryItem}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditInventoryItem", urlMode = UrlMode.INTER_APP, description = "${inventoryItemId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "inventoryItemId")}))
        }
    )
    public interface ProductionRunTaskInventoryProducedList {}

    @Form(
        name = "FindDelivProductContent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "ProductionRunContent",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "productId", entryName = "delivProductId", hidden = @HiddenField),
            @FormField(name = "contentLocale", text = @TextField(defaultValue = "${locale}")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupCustomerName")),
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "CONTENT_USER")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindDelivProductContent {}

    @Form(
        name = "ListProductionRunContent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "productionRunContents",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "drDataResourceId", display = @DisplayField),
            @FormField(name = "workEffortContentTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortContentType", description = "${description}")),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType", description = "${description}")),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "localeString", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drMimeTypeId", display = @DisplayField),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.CommonContent}", display = @DisplayField(description = "default ${drObjectInfo}")),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.CommonContent}", useWhen = "${groovy: drDataResourceTypeId != null && (drDataResourceTypeId.contains(\"FILE\") || drDataResourceTypeId.equals(\"IMAGE_OBJECT\"))}", widgetStyle = "${styles.link_nav_info_value_long}", hyperlink = @HyperlinkField(target = "/content/control/ViewBinaryDataResource", urlMode = UrlMode.CONTENT, description = "${drObjectInfo}", targetWindow = "productionRunContentWindow", parameters = {@ParameterDef(paramName = "dataResourceId", fromField = "drDataResourceId")})),
            @FormField(name = "drObjectInfo", entryName = "drDataResourceId", title = "${uiLabelMap.CommonContent}", useWhen = "${groovy: drDataResourceTypeId != null && drDataResourceTypeId.equals(\"ELECTRONIC_TEXT\")}", displayEntity = @DisplayEntityField(entityName = "ElectronicText", keyFieldName = "dataResourceId", description = "${textData}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductionRunContent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "workEffortContentTypeId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "productionRunId", fromField = "productionRunId")}))
        }
    )
    public interface ListProductionRunContent {}

    @Form(
        name = "ListDelivProductContent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.MULTI,
        target = "createProductionRunContents?productionRunId=${productionRunId}",
        listName = "delivProductContentsForLocaleAndUser",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", entryName = "productionRunId", hidden = @HiddenField),
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "drDataResourceId", display = @DisplayField),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType", description = "${description}")),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "localeString", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drMimeTypeId", display = @DisplayField),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.CommonContent}", display = @DisplayField(description = "default ${drObjectInfo}")),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.CommonContent}", useWhen = "${groovy: drDataResourceTypeId != null && (drDataResourceTypeId.contains(\"FILE\") || drDataResourceTypeId.equals(\"IMAGE_OBJECT\"))}", widgetStyle = "${styles.link_nav_info_value_long}", hyperlink = @HyperlinkField(target = "/content/control/ViewBinaryDataResource", urlMode = UrlMode.CONTENT, description = "${drObjectInfo}", targetWindow = "productionRunContentWindow", parameters = {@ParameterDef(paramName = "dataResourceId", fromField = "drDataResourceId")})),
            @FormField(name = "drObjectInfo", entryName = "drDataResourceId", title = "${uiLabelMap.CommonContent}", useWhen = "${groovy: drDataResourceTypeId != null && drDataResourceTypeId.equals(\"ELECTRONIC_TEXT\")}", displayEntity = @DisplayEntityField(entityName = "ElectronicText", keyFieldName = "dataResourceId", description = "${textData}")),
            @FormField(name = "workEffortContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortContentType", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListDelivProductContent {}

    @Form(
        name = "ProductionRunTaskFixedAssets",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortFixedAssetAssign",
        listName = "records",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortFixedAssetAssign", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "fixedAssetId", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]", subHyperlink = @SubHyperlink(target = "WorkCenterLoad", description = "${uiLabelMap.ManufacturingWorkCenterLoad}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "fixedAssetId")}))),
            @FormField(name = "availabilityStatusId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFA_AVAILABLE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", useWhen = "!\"${declarationScreen}\".equals(\"Y\")", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeWorkEffortFixedAssetAssign", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "productionRunId", fromField = "productionRunId")}))
        }
    )
    public interface ProductionRunTaskFixedAssets {}

    @Form(
        name = "ListProductionRunTaskFixedAssets",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "productionRunFixedAssetsData",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortFixedAssetAssign", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "workEffortId", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "fixedAssetId", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem"))
        }
    )
    public interface ListProductionRunTaskFixedAssets {}

    @Form(
        name = "ListProductionRunNotes",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "productionRunNoteData",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortNoteAndData", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "internalNote", hidden = @HiddenField),
            @FormField(name = "noteId", hidden = @HiddenField),
            @FormField(name = "noteParty", hidden = @HiddenField),
            @FormField(name = "noteDateTime", hidden = @HiddenField)
        }
    )
    public interface ListProductionRunNotes {}

    @Form(
        name = "EditProductionRunTaskFixedAsset",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "EditProductionRun",
        defaultMapName = "fixedAssetData",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortFixedAssetAssign")
        },
        fields = {
            @FormField(name = "actionForm", hidden = @HiddenField(value = "${actionForm}")),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", useWhen = "${actionIsAdd}!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "[${workEffortId}] ${workEffortName}", constraints = {@EntityConstraint(name = "workEffortParentId", value = "${productionRunId}")}))),
            @FormField(name = "workEffortId", useWhen = "${actionIsAdd}==null", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "[${workEffortId}] ${workEffortName}")),
            @FormField(name = "fixedAssetId", useWhen = "${actionIsAdd}!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName}"))),
            @FormField(name = "fixedAssetId", useWhen = "${actionIsAdd}==null", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "${actionIsAdd}==null", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "declarationScreen.equals(\"Y\")", target = "ProductionRunDeclaration")
        }
    )
    public interface EditProductionRunTaskFixedAsset {}

    @Form(
        name = "AddProductionRunTaskFixedAsset",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "createWorkEffortFixedAssetAssign",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortFixedAssetAssign")
        },
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTask}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", constraints = {@EntityConstraint(name = "workEffortParentId", value = "${productionRunId}")}))),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingMachine}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductionRunTaskFixedAsset {}

    @Form(
        name = "ProductionRunWorkers",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "workers",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyName", title = "${uiLabelMap.ManufacturingWorker}", display = @DisplayField),
            @FormField(name = "roleDescription", title = "${uiLabelMap.CommonRole}", display = @DisplayField),
            @FormField(name = "taskLabel", title = "${uiLabelMap.ManufacturingRoutingTask}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField)
        }
    )
    public interface ProductionRunWorkers {}

    @Form(
        name = "AssignProductionRunWorker",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "createProductionRunPartyAssign",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductionRunPartyAssign")
        },
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.ManufacturingWorker}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "EMPLOYEE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTask}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", constraints = {@EntityConstraint(name = "workEffortParentId", value = "${productionRunId}")}))),
            @FormField(name = "fromDate", hidden = @HiddenField),
            @FormField(name = "thruDate", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AssignProductionRunWorker {}

    @Form(
        name = "listShipmentPlan",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "shipmentPlan",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId", fromField = "orderId")})),
            @FormField(name = "orderItemSeqId", title = "${uiLabelMap.ProductOrderItem}", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.ProductQuantity}", display = @DisplayField),
            @FormField(name = "issuedQuantity", display = @DisplayField),
            @FormField(name = "totOrderedQuantity", display = @DisplayField),
            @FormField(name = "notAvailableQuantity", title = "${uiLabelMap.ProductNotAvailable}", display = @DisplayField),
            @FormField(name = "totPlannedQuantity", display = @DisplayField),
            @FormField(name = "totIssuedQuantity", display = @DisplayField),
            @FormField(name = "productionRunId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ShowProductionRun", description = "${productionRunId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productionRunId", fromField = "productionRunId")})),
            @FormField(name = "productionRunStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "productionRunEstimatedCompletionDate", display = @DisplayField),
            @FormField(name = "productionRunQuantityProduced", display = @DisplayField)
        }
    )
    public interface listShipmentPlan {}

    @Form(
        name = "listShipmentPlans",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "shipmentPlans",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "/facility/control/EditShipment", urlMode = UrlMode.INTER_APP, description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId", fromField = "shipmentId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "estimatedShipDate", sortField = true, display = @DisplayField),
            @FormField(name = "viewShipmentPlanAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "WorkWithShipmentPlans", description = "${uiLabelMap.CommonView}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentId")}))
        }
    )
    public interface listShipmentPlans {}

    @Form(
        name = "SelectMrpName",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "ManufacturingReports",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "mrpName", title = "${uiLabelMap.ManufacturingMrpName}", text = @TextField(size = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        }
    )
    public interface SelectMrpName {}

    @Form(
        name = "linkProductionRun",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "createProductionRunAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortAssocTypeId", hidden = @HiddenField(value = "WORK_EFF_PRECEDENCY")),
            @FormField(name = "productionRunIdTo", title = "${uiLabelMap.ManufacturingProductionRunId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "[ID: ${workEffortId}] - ${workEffortName}", keyFieldName = "workEffortId", constraints = {@EntityConstraint(name = "workEffortPurposeTypeId", value = "WEPT_PRODUCTION_RUN")}, orderBy = {@EntityOrderBy(fieldName = "workEffortId")}))),
            @FormField(name = "workFlowSequenceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORKFLOW")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface linkProductionRun {}

    @Form(
        name = "mandatoryWorkEfforts",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "mandatoryWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortIdFrom", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ShowProductionRun", description = "${workEffortIdFrom}", alsoHidden = false, linkType = "anchor", parameters = {@ParameterDef(paramName = "productionRunId", fromField = "workEffortIdFrom")})),
            @FormField(name = "workEffortName", entryName = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingTaskName}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName} ", cache = true)),
            @FormField(name = "quantityToProduce", entryName = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingQuantityToProduce}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${quantityToProduce} ", cache = true)),
            @FormField(name = "quantityProduced", entryName = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingQuantityProduced}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${quantityProduced} ", cache = true)),
            @FormField(name = "estimatedStartDate", entryName = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingStartDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${estimatedStartDate} ", cache = true)),
            @FormField(name = "estimatedCompletionDate", entryName = "workEffortIdFrom", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${estimatedCompletionDate} ", cache = true)),
            @FormField(name = "actualStartDate", entryName = "workEffortIdFrom", title = "${uiLabelMap.CommonActualStartDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${actualStartDate} ", cache = true)),
            @FormField(name = "actualCompletionDate", entryName = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingActualCompletionDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${actualCompletionDate} ", cache = true))
        }
    )
    public interface mandatoryWorkEfforts {}

    @Form(
        name = "dependentWorkEfforts",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "dependentWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortIdTo", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ShowProductionRun", description = "${workEffortIdTo}", alsoHidden = false, linkType = "anchor", parameters = {@ParameterDef(paramName = "productionRunId", fromField = "workEffortIdTo")})),
            @FormField(name = "workEffortName", entryName = "workEffortIdTo", title = "${uiLabelMap.ManufacturingTaskName}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName} ", cache = true)),
            @FormField(name = "quantityToProduce", entryName = "workEffortIdTo", title = "${uiLabelMap.ManufacturingQuantityToProduce}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${quantityToProduce} ", cache = true)),
            @FormField(name = "quantityProduced", entryName = "workEffortIdTo", title = "${uiLabelMap.ManufacturingQuantityProduced}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${quantityProduced} ", cache = true)),
            @FormField(name = "estimatedStartDate", entryName = "workEffortIdTo", title = "${uiLabelMap.ManufacturingStartDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${estimatedStartDate} ", cache = true)),
            @FormField(name = "estimatedCompletionDate", entryName = "workEffortIdTo", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${estimatedCompletionDate} ", cache = true)),
            @FormField(name = "actualStartDate", entryName = "workEffortIdTo", title = "${uiLabelMap.CommonActualStartDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${actualStartDate} ", cache = true)),
            @FormField(name = "actualCompletionDate", entryName = "workEffortIdTo", title = "${uiLabelMap.ManufacturingActualCompletionDate}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${actualCompletionDate} ", cache = true))
        }
    )
    public interface dependentWorkEfforts {}

    @Form(
        name = "ProductionRunTaskCosts",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "taskCosts",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CostComponent", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "geoId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "costComponentTypeId", displayEntity = @DisplayEntityField(entityName = "CostComponentType")),
            @FormField(name = "costComponentCalcId", displayEntity = @DisplayEntityField(entityName = "CostComponentCalc"))
        }
    )
    public interface ProductionRunTaskCosts {}

    @Form(
        name = "AddProductionRunComponent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "addProductionRunComponent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addProductionRunComponent")
        },
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTask}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", constraints = {@EntityConstraint(name = "workEffortParentId", envName = "productionRunId")}, orderBy = {@EntityOrderBy(fieldName = "workEffortId")}))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductionRunComponent {}

    @Form(
        name = "ProductionRunTaskComponents",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "updateProductionRunComponent",
        listName = "records",
        paginateTarget = "ProductionRunComponents",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "internalName", mapName = "product", title = "${uiLabelMap.ProductInternalName}", display = @DisplayField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "estimatedQuantity", text = @TextField),
            @FormField(name = "quantityUomId", mapName = "product", title = "${uiLabelMap.CommonUom}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${abbreviation}")),
            @FormField(name = "qoh", title = "${uiLabelMap.ManufacturingQoh}", display = @DisplayField),
            @FormField(name = "atp", title = "${uiLabelMap.ManufacturingAtp}", display = @DisplayField),
            @FormField(name = "shortage", title = " ", useWhen = "\"Y\".equals(\"${shortage}\")", widgetStyle = "${styles.text_color_alert}", display = @DisplayField(description = "${uiLabelMap.ManufacturingComponentShortage}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductionRunComponent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "workEffortGoodStdTypeId"), @ParameterDef(paramName = "productionRunId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product", useCache = true)})
    )
    public interface ProductionRunTaskComponents {}

    @Form(
        name = "ReplaceProductionRunComponent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.MULTI,
        target = "replaceProductionRunComponent",
        listName = "records",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "newProductId", title = "${uiLabelMap.ManufacturingReplaceWithProduct}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "_rowSubmit", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.ManufacturingReplace}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ReplaceProductionRunComponent {}

    @Form(
        name = "ProductionRunTaskActualComponents",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        target = "updateProductionRunComponent",
        listName = "records",
        paginateTarget = "ProductionRunActualComponents",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditInventoryItem", urlMode = UrlMode.INTER_APP, description = "${inventoryItemId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "inventoryItemId")})),
            @FormField(name = "productId", entryName = "inventoryItemId", displayEntity = @DisplayEntityField(entityName = "InventoryItem", keyFieldName = "inventoryItemId", description = "${productId}")),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "quantityOnHandDiff", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "unitCost", entryName = "inventoryItemId", displayEntity = @DisplayEntityField(entityName = "InventoryItem", keyFieldName = "inventoryItemId", description = "${unitCost}")),
            @FormField(name = "reasonEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "description", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "quantityOnHandDiff", value = "${groovy:-1*quantityOnHandDiff}", type = "BigDecimal")})
    )
    public interface ProductionRunTaskActualComponents {}

    @Form(
        name = "IssueProductionRunComponent",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "issueProductionRunTaskComponent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "issueProductionRunTaskComponent")
        },
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", ignored = @IgnoredField),
            @FormField(name = "reserveOrderEnumId", ignored = @IgnoredField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingTaskId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", constraints = {@EntityConstraint(name = "workEffortParentId", envName = "productionRunId")}, orderBy = {@EntityOrderBy(fieldName = "workEffortId")}))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", text = @TextField),
            @FormField(name = "reasonEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "IID_REASON")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface IssueProductionRunComponent {}

}
