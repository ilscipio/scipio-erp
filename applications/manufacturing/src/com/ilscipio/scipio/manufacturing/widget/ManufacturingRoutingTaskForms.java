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
public class ManufacturingRoutingTaskForms {

    @Form(
        name = "FindRoutings",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "FindRouting",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingId}", textFind = @TextFindField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingRoutingName}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindRoutings {}

    @Form(
        name = "ListRoutings",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindRouting",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRouting", description = "${workEffortId}", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingRoutingName}", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantityToProduce}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "WorkEffort")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListRoutings {}

    @Form(
        name = "EditRouting",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "UpdateRouting",
        defaultMapName = "routing",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortTypeId", useWhen = "routing==null", hidden = @HiddenField(value = "ROUTING")),
            @FormField(name = "currentStatusId", useWhen = "routing==null", hidden = @HiddenField(value = "ROU_ACTIVE")),
            @FormField(name = "workEffortId", useWhen = "routing!=null", hidden = @HiddenField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingRoutingName}", requiredField = true, text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantityToProduce}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "routing==null", target = "CreateRouting")
        }
    )
    public interface EditRouting {}

    @Form(
        name = "FindRoutingTasks",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "FindRoutingTask",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTaskId}", textFind = @TextFindField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", textFind = @TextFindField),
            @FormField(name = "fixedAssetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]", constraints = {@EntityConstraint(name = "fixedAssetTypeId", value = "GROUP_EQUIPMENT")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindRoutingTasks {}

    @Form(
        name = "ListRoutingTasks",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindRoutingTask",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingTaskId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRoutingTask", description = "${workEffortId}", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.ManufacturingTaskPurpose}", displayEntity = @DisplayEntityField(entityName = "WorkEffortPurposeType")),
            @FormField(name = "fixedAssetId", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}")),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "estimatedMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "WorkEffort")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListRoutingTasks {}

    @Form(
        name = "EditRoutingTask",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "UpdateRoutingTask",
        defaultMapName = "routingTask",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortTypeId", useWhen = "routingTask==null", hidden = @HiddenField(value = "ROU_TASK")),
            @FormField(name = "currentStatusId", useWhen = "routingTask==null", hidden = @HiddenField(value = "ROU_ACTIVE")),
            @FormField(name = "workEffortId", useWhen = "routingTask!=null", hidden = @HiddenField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", requiredField = true, text = @TextField),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.ManufacturingTaskPurpose}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortPurposeType", description = "${description}", constraints = {@EntityConstraint(name = "workEffortPurposeTypeId", value = "ROU%", operator = "like")}))),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "fixedAssetId", tooltip = "${uiLabelMap.ManufacturingRoutingTaskFixedAssetTooltip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]", constraints = {@EntityConstraint(name = "fixedAssetTypeId", value = "GROUP_EQUIPMENT")}))),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", text = @TextField),
            @FormField(name = "estimatedMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", text = @TextField),
            @FormField(name = "estimateCalcMethod", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "TASK_FORMULA")}))),
            @FormField(name = "reservPersons", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "routingTask==null", target = "CreateRoutingTask")
        }
    )
    public interface EditRoutingTask {}

    @Form(
        name = "ListRoutingTaskCosts",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        listName = "allCosts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortCostCalc", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "costComponentTypeId", displayEntity = @DisplayEntityField(entityName = "CostComponentType")),
            @FormField(name = "costComponentCalcId", displayEntity = @DisplayEntityField(entityName = "CostComponentCalc")),
            @FormField(name = "cancelWorkEffortCostCalcAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "removeRoutingTaskCost", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "costComponentCalcId"), @ParameterDef(paramName = "costComponentTypeId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortId")}))
        }
    )
    public interface ListRoutingTaskCosts {}

    @Form(
        name = "AddRoutingTaskCost",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "addRoutingTaskCost",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortCostCalc", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "costComponentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CostComponentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", envName = "nullField")}))),
            @FormField(name = "costComponentCalcId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CostComponentCalc", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddRoutingTaskCost {}

    @Form(
        name = "ListRoutingTaskRoutings",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        listName = "allRoutings",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortIdFrom", title = "${uiLabelMap.ManufacturingRouting}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "[${workEffortId}] ${workEffortName}")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRouting", description = "${uiLabelMap.ManufacturingEditRouting}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdFrom")}))
        }
    )
    public interface ListRoutingTaskRoutings {}

    @Form(
        name = "ListRoutingTaskAssoc",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        target = "EditRoutingTaskAssoc",
        listName = "allRoutingTasks",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortAssoc", mapName = "routingTaskAssoc")
        },
        fields = {
            @FormField(name = "workEffortIdFrom", hidden = @HiddenField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "workEffortIdTo", title = "${uiLabelMap.ManufacturingTaskName}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditRoutingTask", description = "[${workEffortIdTo}] ${workEffortToName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdTo")})),
            @FormField(name = "workEffortAssocTypeId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "workEffortToSetup", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "workEffortToRun", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveRoutingTaskAssoc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdFrom"), @ParameterDef(paramName = "workEffortIdFrom"), @ParameterDef(paramName = "workEffortIdTo"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortAssocTypeId", value = "ROUTING_COMPONENT")}))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortIdFrom"), @SortField(name = "workEffortIdTo"), @SortField(name = "sequenceNum")})
    )
    public interface ListRoutingTaskAssoc {}

    @Form(
        name = "UpdateRoutingTaskAssoc",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "UpdateRoutingTaskAssoc",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortAssoc", mapName = "routingTaskAssoc")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${workEffortIdFrom}")),
            @FormField(name = "workEffortIdFrom", hidden = @HiddenField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}"),
            @FormField(name = "workEffortIdTo", title = "${uiLabelMap.ManufacturingRoutingTaskId}", display = @DisplayField(description = "${routingTask.workEffortName}")),
            @FormField(name = "workEffortAssocTypeId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortIdFrom"), @SortField(name = "workEffortIdTo"), @SortField(name = "sequenceNum")})
    )
    public interface UpdateRoutingTaskAssoc {}

    @Form(
        name = "EditRoutingProductLink",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "UpdateRoutingProductLink",
        defaultMapName = "routingProductLink",
        headerRowStyle = "header-row",
        defaultTableStyle = "basic-table",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortGoodStandard", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "workEffortGoodStdTypeId", hidden = @HiddenField(value = "ROU_PROD_TEMPLATE")),
            @FormField(name = "productId", useWhen = "routingProductLink!=null", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", useWhen = "routingProductLink==null", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFromDate}", useWhen = "routingProductLink!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThruDate}"),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}"),
            @FormField(name = "submitButton", title = "${uiLabelMap.CommonUpdate}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "routingProductLink==null", target = "AddRoutingProductLink")
        }
    )
    public interface EditRoutingProductLink {}

    @Form(
        name = "ListRoutingProductLink",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        target = "EditRoutingProductLink",
        listName = "allRoutingProductLinks",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        defaultTableStyle = "basic-table hover-bar",
        fields = {
            @FormField(name = "productId", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProductManufacturing", urlMode = UrlMode.INTER_APP, description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productName", entryName = "productId", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFromDate}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThruDate}", display = @DisplayField),
            @FormField(name = "estimatedQuantity", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField),
            @FormField(name = "editLink", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRoutingProductLink", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortGoodStdTypeId", value = "ROU_PROD_TEMPLATE")})),
            @FormField(name = "deleteLink", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeRoutingProductLink", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortGoodStdTypeId", value = "ROU_PROD_TEMPLATE")}))
        }
    )
    public interface ListRoutingProductLink {}

    @Form(
        name = "ListRoutingTaskProducts",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        target = "ListRoutingTaskProducts",
        listName = "allProducts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${productId} ${internalName}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRoutingTaskProduct", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortGoodStdTypeId", value = "PRUNT_PROD_DELIV")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeRoutingTaskProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortGoodStdTypeId", value = "PRUNT_PROD_DELIV")}))
        }
    )
    public interface ListRoutingTaskProducts {}

    @Form(
        name = "EditRoutingTaskProduct",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "updateRoutingTaskProduct",
        defaultMapName = "routingProductAction",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortGoodStandard", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "estimatedQuantity", hidden = @HiddenField),
            @FormField(name = "estimatedCost", hidden = @HiddenField),
            @FormField(name = "workEffortGoodStdTypeId", hidden = @HiddenField(value = "PRUNT_PROD_DELIV")),
            @FormField(name = "productId", useWhen = "routingProductLink!=null", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", useWhen = "routingProductLink==null", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "routingProductLink!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "routingProductLink==null", target = "addRoutingTaskProduct")
        }
    )
    public interface EditRoutingTaskProduct {}

    @Form(
        name = "ListRoutingTaskFixedAssets",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        type = FormType.LIST,
        target = "updateRoutingTaskFixedAsset",
        listName = "allFixedAssets",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortFixedAssetStd")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fixedAssetTypeId", displayEntity = @DisplayEntityField(entityName = "FixedAssetType")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeRoutingTaskFixedAsset", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "fixedAssetTypeId")}))
        }
    )
    public interface ListRoutingTaskFixedAssets {}

    @Form(
        name = "EditRoutingTaskFixedAsset",
        location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml",
        target = "createRoutingTaskFixedAsset",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortFixedAssetStd", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fixedAssetTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FixedAssetType", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditRoutingTaskFixedAsset {}

}
