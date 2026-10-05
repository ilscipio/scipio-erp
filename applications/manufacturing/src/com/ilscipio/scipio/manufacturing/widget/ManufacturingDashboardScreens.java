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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Manufacturing capacity planning dashboard widget definitions.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingDashboardScreens {

    @Screen(name = "Dashboard", location = "component://manufacturing/widget/manufacturing/DashboardScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingDashboard")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SERVICE, serviceName = "getManufacturingDashboard", resultMapName = "dashboardResult", fieldMaps = {
        @FieldMap(fieldName = "facilityId", fromField = "parameters.facilityId"),
        @FieldMap(fieldName = "days", fromField = "parameters.days")
    })
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FacilitySelect", location = "component://manufacturing/widget/manufacturing/DashboardForms.xml"),
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/dashboard/Dashboard.ftl")
            })
        }
    )
    public interface Dashboard {}

    @Screen(name = "WorkCenterLoad", location = "component://manufacturing/widget/manufacturing/DashboardScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingWorkCenterLoad")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkCenterLoad")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkCenterLoad", resultMapName = "wclResult", fieldMaps = {
        @FieldMap(fieldName = "fixedAssetId", fromField = "parameters.fixedAssetId"),
        @FieldMap(fieldName = "facilityId", fromField = "parameters.facilityId"),
        @FieldMap(fieldName = "fromDate", fromField = "parameters.fromDate"),
        @FieldMap(fieldName = "thruDate", fromField = "parameters.thruDate"),
        @FieldMap(fieldName = "includeClosed", fromField = "parameters.includeClosed")
    })
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "WorkCenterLoadForm", location = "component://manufacturing/widget/manufacturing/DashboardForms.xml"),
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/planning/WorkCenterLoad.ftl")
            })
        }
    )
    public interface WorkCenterLoad {}

    @Screen(name = "ReportsHub", location = "component://manufacturing/widget/manufacturing/DashboardScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingReports")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ManufacturingReports")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/dashboard/ReportsHub.ftl")
            })
        }
    )
    public interface ReportsHub {}

}
