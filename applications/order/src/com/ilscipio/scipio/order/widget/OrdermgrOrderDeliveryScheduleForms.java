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
public class OrdermgrOrderDeliveryScheduleForms {

    @Form(
        name = "UpdateDeliveryScheduleInformation",
        location = "component://order/widget/ordermgr/OrderDeliveryScheduleForms.xml",
        target = "updateOrderDeliverySchedule",
        defaultMapName = "schedule",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateOrderDeliverySchedule")
        },
        fields = {
            @FormField(name = "orderId", hidden = @HiddenField),
            @FormField(name = "orderItemSeqId", hidden = @HiddenField),
            @FormField(name = "totalWeightUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "WEIGHT_MEASURE")}))),
            @FormField(name = "totalCubicUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "VOLUME_DRY_MEASURE")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_DEL_SCH")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "schedule==null", target = "createOrderDeliverySchedule")
        }
    )
    public interface UpdateDeliveryScheduleInformation {}

}
