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
package com.ilscipio.scipio.accounting.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AccountingScreens {

    @Screen(name = "main", location = "component://accounting/widget/AccountingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}", global = true)
    @Action(type = ActionType.SET, field = "viewSize", value = "10")
    @Action(type = ActionType.SET, field = "titleProperty", value = "Accounting")
    @DecoratorScreen(
        name = "CommonAccountingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ListPayments", location = "component://accounting/widget/payments/PaymentScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioIncomesExpenses", location = "${parameters.mainDecoratorLocation}"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ApPastDueInvoices", location = "${parameters.mainDecoratorLocation}"
                        )}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ArPastDueInvoices", location = "${parameters.mainDecoratorLocation}"
                        )})}),
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}"
                        )})})
        }
    )
    public interface main {}

    @Screen(name = "ScpEgltCommon.js", location = "component://accounting/widget/AccountingScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getUserPreferenceGroup", resultMapName = "prefResult", fieldMaps = {@FieldMap(fieldName = "userPrefGroupTypeId", value = "GLOBAL_PREFERENCES")})
    @Action(type = ActionType.SET, field = "userPreferences", fromField = "prefResult.userPrefMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/static/ScpEgltCommon.js.ftl")}))
    public interface ScpEgltCommon_js {}

}
