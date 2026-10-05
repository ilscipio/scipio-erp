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
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ManufacturingBomScreens {

    @Screen(name = "CommonBomDecorator", location = "component://manufacturing/widget/manufacturing/BomScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Bom")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonBomDecorator {}

    @Screen(name = "EditProductBom", location = "component://manufacturing/widget/manufacturing/BomScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductBom")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductBom")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/bom/EditProductBom.groovy")
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/bom/EditProductBom.ftl"
            )})
        }
    )
    public interface EditProductBom {}

    @Screen(name = "EditProductManufacturingRules", location = "component://manufacturing/widget/manufacturing/BomScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductManufacturingRules")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productManufacturingRules")
    @Action(type = ActionType.SET, field = "ruleId", fromField = "parameters.ruleId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductManufacturingRule", valueField = "manufacturingRule")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductManufacturingRule", list = "manufacturingRules", orderBy = {"ruleId", "fromDate"})
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductManufacturingRules", location = "component://manufacturing/widget/manufacturing/BomForms.xml"
            )}, screenlets = {
                @Screenlet(name = "EditProductManufacturingRulePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "UpdateProductManufacturingRule", location = "component://manufacturing/widget/manufacturing/BomForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditProductManufacturingRules {}

    @Screen(name = "BomSimulation", location = "component://manufacturing/widget/manufacturing/BomScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingBomSimulation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "bomSimulation")
    @Action(type = ActionType.SET, field = "bomType", fromField = "parameters.bomType")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SET, field = "type", fromField = "parameters.type")
    @Action(type = ActionType.SET, field = "quantity", fromField = "parameters.quantity")
    @Action(type = ActionType.SET, field = "amount", fromField = "parameters.amount")
    @Action(type = ActionType.SET, field = "productFeatureApplTypeId", value = "STANDARD_FEATURE")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductFeatureAndAppl", list = "selectedFeatures", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "productId"), @FieldMap(fieldName = "productFeatureApplTypeId", fromField = "productFeatureApplTypeId")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/bom/BomSimulation.groovy")
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/bom/BomSimulation.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "RunBomSimulation", location = "component://manufacturing/widget/manufacturing/BomForms.xml"
                )}, position = 0)})
        }
    )
    public interface BomSimulation {}

    @Screen(name = "FindBom", location = "component://manufacturing/widget/manufacturing/BomScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindBom")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductBom")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "findBom")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/bom/FindProductBom.groovy")
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.alert_wrap} ${styles.alert_type_info}", labels = {
                    @Label(text = "${uiLabelMap.ManufacturingBomSetupGuideTitle}", style = "h4"),
                    @Label(text = "${uiLabelMap.ManufacturingBomSetupStep1}", style = "p"),
                    @Label(text = "${uiLabelMap.ManufacturingBomSetupStep2}", style = "p"),
                    @Label(text = "${uiLabelMap.ManufacturingBomSetupStep3}", style = "p"),
                    @Label(text = "${uiLabelMap.ManufacturingBomSetupStep4}", style = "p")
                })
            }, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "findBom", location = "component://manufacturing/widget/manufacturing/BomForms.xml")
                }),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListBom", location = "component://manufacturing/widget/manufacturing/BomForms.xml")
                })
            })
        }
    )
    public interface FindBom {}

}
