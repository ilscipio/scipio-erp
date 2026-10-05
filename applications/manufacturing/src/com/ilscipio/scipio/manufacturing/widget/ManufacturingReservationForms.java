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

/**
 * Lot reservation by weight: a form to reserve a lot for a production run task, and a list form to show and
 * release the run's open lot reservations.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ManufacturingReservationForms {

    @Form(
        name = "ReserveProductionRunLot",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        target = "reserveProductionRunLot",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.ManufacturingRoutingTask}", requiredField = true,
                dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffort",
                    description = "${workEffortName} [${workEffortId}]",
                    constraints = {@EntityConstraint(name = "workEffortParentId", envName = "productionRunId")},
                    orderBy = {@EntityOrderBy(fieldName = "workEffortId")}))),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", requiredField = true,
                dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortAndGoods",
                    description = "${productId}",
                    constraints = {
                        @EntityConstraint(name = "workEffortParentId", envName = "productionRunId"),
                        @EntityConstraint(name = "workEffortGoodStdTypeId", value = "PRUNT_PROD_NEEDED")
                    },
                    orderBy = {@EntityOrderBy(fieldName = "productId")}))),
            @FormField(name = "lotId", title = "${uiLabelMap.ManufacturingLot}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.ManufacturingReserveALot}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface ReserveProductionRunLot {}

    @Form(
        name = "ProductionRunLotReservations",
        location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml",
        type = FormType.LIST,
        listName = "reservations",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productionRunId", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "taskName", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField),
            @FormField(name = "lotId", title = "${uiLabelMap.ManufacturingLot}", display = @DisplayField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField),
            @FormField(name = "inventoryItemId", title = "${uiLabelMap.ProductInventoryItemId}", display = @DisplayField),
            @FormField(name = "quantityReserved", title = "${uiLabelMap.ManufacturingReservedQuantity}", display = @DisplayField),
            @FormField(name = "uomAbbreviation", title = "${uiLabelMap.CommonUom}", display = @DisplayField),
            @FormField(name = "reservedDate", title = "${uiLabelMap.ManufacturingReservedDate}", display = @DisplayField),
            @FormField(name = "releaseAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}",
                hyperlink = @HyperlinkField(target = "releaseProductionRunLot", description = "${uiLabelMap.CommonRelease}", alsoHidden = false,
                    parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "productionRunId")}))
        }
    )
    public interface ProductionRunLotReservations {}

}
