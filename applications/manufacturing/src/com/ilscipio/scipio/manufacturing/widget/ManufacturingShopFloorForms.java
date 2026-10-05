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
 * Form definitions for the manufacturing shop floor task declaration feature.
 *
 * <p>SCIPIO: 4.0.0: Added for the shop floor screen.</p>
 */
public class ManufacturingShopFloorForms {

    @Form(
        name = "ShopFloorFilter",
        location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml",
        target = "ShopFloor",
        method = "get",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingWorkCenter}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]", constraints = {@EntityConstraint(name = "fixedAssetTypeId", value = "PRODUCTION_EQUIPMENT,GROUP_EQUIPMENT", operator = "in")}))),
            @FormField(name = "facilityId", title = "${uiLabelMap.CommonFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "includeCompleted", title = "${uiLabelMap.ManufacturingIncludeCompleted}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface ShopFloorFilter {}

    @Form(
        name = "CreateProductionRunFromOrder",
        location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml",
        target = "createProductionRunsFromOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "orderId", title = "${uiLabelMap.CommonOrder}", requiredField = true, text = @TextField(size = 20)),
            @FormField(name = "shipGroupSeqId", title = "${uiLabelMap.ManufacturingShipGroup}", text = @TextField(size = 5, defaultValue = "00001")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField(size = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductionRunFromOrder {}

    @Form(
        name = "ListProductionRunRejects",
        location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml",
        type = FormType.LIST,
        listName = "rejects",
        oddRowStyle = "alternate-row",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "taskName", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "reasonDescription", title = "${uiLabelMap.ManufacturingRejectReason}", display = @DisplayField),
            @FormField(name = "lotId", title = "${uiLabelMap.ManufacturingLot}", display = @DisplayField),
            @FormField(name = "rejectDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "userLoginId", title = "${uiLabelMap.ManufacturingUser}", display = @DisplayField),
            @FormField(name = "comments", title = "${uiLabelMap.ManufacturingComments}", display = @DisplayField)
        }
    )
    public interface ListProductionRunRejects {}

}
