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
public class FacilityCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.FacilityCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.FacilityCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "facilitymgr", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "FacilityAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://product/widget/facility/FacilityMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.ProductFacility}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonFacilityAppDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonFacilityAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonFacilityAppSideBarMenu", location = "component://product/widget/facility/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonFacilityAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonFacilityAppDecorator {}

    @Screen(name = "CommonFacilityDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#Facility")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"facility", "displayWithNoFacility"})}))
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifNotEmpty = {"facility", "displayWithNoFacility"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductErrorFacilityIdNotFound}", style = "common-msg-error"
                )}))})
        }
    )
    public interface CommonFacilityDecorator {}

    @Screen(name = "CommonFacilityScheduleDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Scheduling")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/FacilityScheduleTabBar.ftl"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonFacilityScheduleDecorator {}

    @Screen(name = "CommonFacilitySettingsDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#FacilitySettings")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFacilitySettingsDecorator {}

    @Screen(name = "CommonFacilityInventoryDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#FacilityInventory")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFacilityInventoryDecorator {}

    @Screen(name = "CommonFacilityPickingDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#FacilityPicking")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFacilityPickingDecorator {}

    @Screen(name = "CommonFacilityPackOrderDecorator", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#FacilityPackOrder")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFacilityPackOrderDecorator {}

    @Screen(name = "main", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFacilityManager")
    @Action(type = ActionType.SET, field = "parameters.lookupFlag", value = "Y")
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.CommonRecentActivity}", containers = {
                    @ContainerInScreenlet(style = "${styles.grid_row}", sections = {
                        @SectionLeaf(actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "parameters.shipmentTypeId", value = "PURCHASE_SHIPMENT"
                        
                    ),
                        @Action(type = ActionType.SET, field = "sectionTitle", value = "${uiLabelMap.ProductIncomingShipments}"
                    
                ),
                    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/FindShipment.groovy"
                
            )}), widgets = @WidgetsLeaf(containers = {
                    @ContainerLeaf(style = "${styles.grid_large}6 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://product/webapp/facility/dashboard/FacilityShipments.ftl"
                    
            )})})),
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "parameters.shipmentTypeId", value = "SALES_SHIPMENT"
                    
            ),
                    @Action(type = ActionType.SET, field = "sectionTitle", value = "${uiLabelMap.ProductOutgoingShipments}"
                
            ),
                @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/FindShipment.groovy"
                
            )}), widgets = @WidgetsLeaf(containers = {
                    @ContainerLeaf(style = "${styles.grid_large}6 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://product/webapp/facility/dashboard/FacilityShipments.ftl"
                    
            )})}))})})})})})
        }
    )
    public interface main {}

    @Screen(name = "MainSideBarMenu", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://product/widget/facility/FacilityMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "FacilityAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://product/widget/facility/FacilityMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonFacilityAppSideBarMenu", location = "component://product/widget/facility/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonFacilityAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonFacilityAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonFacilityAppSideBarMenu {}

}
