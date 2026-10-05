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

/**
 * Fabrication order forms: find/list, the combined create-or-update header form, the runs table with
 * a remove link, and the add-existing-run / create-run-in-order forms.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingFabricationForms {

    @Form(
        name = "findFabricationOrder",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        target = "FindFabricationOrders",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "FAB_ORDER")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingFabricationOrder}", textFind = @TextFindField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId"))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PRODUCTION_RUN")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findFabricationOrder {}

    @Form(
        name = "ListFabricationOrders",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindFabricationOrders",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingFabricationOrderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFabricationOrder", description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fabricationOrderId", fromField = "workEffortId")})),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingFabricationOrder}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", display = @DisplayField),
            @FormField(name = "derivedStatus", title = "${uiLabelMap.CommonStatus}", display = @DisplayField),
            @FormField(name = "runCount", title = "${uiLabelMap.ManufacturingRunCount}", display = @DisplayField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", value = "WorkEffort"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "orderBy", value = "estimatedStartDate")})}),
        rowActions = @RowActions(
            service = {@ServiceAction(serviceName = "getFabricationOrder", resultMapName = "fabSummary", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "fabricationOrderId", fromField = "workEffortId")})},
            set = {
                @SetAction(field = "derivedStatus", value = "${fabSummary.totals.derivedStatus}"),
                @SetAction(field = "runCount", value = "${fabSummary.totals.runCount}", type = "Integer")
            }
        )
    )
    public interface ListFabricationOrders {}

    @Form(
        name = "UpdateFabricationOrder",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        target = "updateFabricationOrder",
        defaultMapName = "fabOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fabricationOrderId", useWhen = "fabOrder!=null", entryName = "workEffortId", hidden = @HiddenField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingFabricationOrder}", requiredField = true, text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId"))),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", dateTime = @DateTimeField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "fabOrder==null", target = "createFabricationOrder")
        }
    )
    public interface UpdateFabricationOrder {}

    @Form(
        name = "ListFabricationOrderRuns",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        type = FormType.LIST,
        listName = "fabOrderRuns",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productionRunId", title = "${uiLabelMap.ManufacturingProductionRunId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ShowProductionRun", description = "${productionRunId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productionRunId", fromField = "productionRunId")})),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantity}", display = @DisplayField),
            @FormField(name = "quantityProduced", title = "${uiLabelMap.ManufacturingQuantityProduced}", display = @DisplayField),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.ManufacturingStartDate}", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.ManufacturingEstimatedCompletionDate}", display = @DisplayField),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductionRunFromFabricationOrder", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productionRunId", fromField = "productionRunId"), @ParameterDef(paramName = "fabricationOrderId", value = "${fabricationOrderId}")}))
        }
    )
    public interface ListFabricationOrderRuns {}

    @Form(
        name = "AddProductionRunToFabricationOrder",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        target = "addProductionRunToFabricationOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fabricationOrderId", hidden = @HiddenField(value = "${fabricationOrderId}")),
            @FormField(name = "productionRunId", title = "${uiLabelMap.ManufacturingProductionRunId}", requiredField = true, text = @TextField(size = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductionRunToFabricationOrder {}

    @Form(
        name = "CreateProductionRunInFabricationOrder",
        location = "component://manufacturing/widget/manufacturing/FabricationForms.xml",
        target = "createProductionRunInFabricationOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fabricationOrderId", hidden = @HiddenField(value = "${fabricationOrderId}")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProduct", size = 16)),
            @FormField(name = "quantity", title = "${uiLabelMap.ManufacturingQuantity}", requiredField = true, text = @TextField(size = 6)),
            @FormField(name = "startDate", title = "${uiLabelMap.ManufacturingStartDate}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId"))),
            @FormField(name = "routingId", title = "${uiLabelMap.ManufacturingRoutingId}", lookup = @LookupField(targetFormName = "LookupRouting", size = 16)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingProductionRunName}", text = @TextField(size = 30)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductionRunInFabricationOrder {}

}
