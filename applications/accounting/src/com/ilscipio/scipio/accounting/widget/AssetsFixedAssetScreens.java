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
public class AssetsFixedAssetScreens {

    @Screen(name = "ListFixedAssets", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindFixedAssets")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssets")
    @DecoratorScreen(
        name = "CommonFixedAssetsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(condition = @Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "CREATE"
                    }), widgets = @WidgetsLeaf(containers = {
                        @ContainerLeaf(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewFixedAsset}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFixedAsset"
                        )})}))})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFixedAssetOptions", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "FixedAssetSearchResults"
                        )}))})})
        }
    )
    public interface ListFixedAssets {}

    @Screen(name = "FixedAssetSearchResults", location = "component://accounting/widget/assets/FixedAssetScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssets", location = "component://accounting/widget/assets/FixedAssetForms.xml")}))
    public interface FixedAssetSearchResults {}

    @Screen(name = "EditFixedAsset", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAsset")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.fixedAsset ? 'PageTitleEditFixedAsset' : 'AccountingNewFixedAsset'}")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditFixedAsset", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "CREATE"
                }),
                @Condition(type = NotEmpty.class, params = {"fixedAsset"})}), widgets = @InlineWidgets(containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewFixedAsset}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFixedAsset"
                    )})}), position = 0)})
        }
    )
    public interface EditFixedAsset {}

    @Screen(name = "ListFixedAssetProducts", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListFixedAssetProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssetProducts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "AccountingFixedAssetProducts")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FixedAssetProduct", list = "fixedAssetProducts", conditions = {@ConditionExpr(fieldName = "fixedAssetId", fromField = "fixedAssetId")}, orderBy = {"productId", "fixedAssetProductTypeId", "fromDate"})
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetProducts", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingFixedAssetProductAdd}", name = "add-fixedasset-product", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetProduct", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListFixedAssetProducts {}

    @Screen(name = "WorkEffortSummary", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssetCalendar")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleWorkEffortRelatedSummary")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "workEffort.fixedAssetId")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "WorkEffortSummary", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )})
        }
    )
    public interface WorkEffortSummary {}

    @Screen(name = "EditFixedAssetStdCosts", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetStdCosts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAssetStdCosts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetStdCosts")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetStdCosts", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetStdCost}", name = "add-fixed-asset-std-cost", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditFixedAssetStdCost", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFixedAssetStdCosts {}

    @Screen(name = "EditFixedAssetIdents", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetIdents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAssetIdents")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetIdents")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "fixedAssetIdentTypeId", fromField = "parameters.fixedAssetIdentTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetIdents", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetIdent}", name = "edit-fixed-asset-idents", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetIdent", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFixedAssetIdents {}

    @Screen(name = "Calendar", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssetCalendar")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "day"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCalendarDay")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"parameters.period"}, ifCompare = {@IfCompare(field = "parameters.period", operator = "equals", value = "week")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCalendarWeek")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "month"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCalendarMonth")}))
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "Calendar", location = "component://workeffort/widget/CalendarScreens.xml"
            )})
        }
    )
    public interface Calendar {}

    @Screen(name = "EditFixedAssetRegistrations", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetRegistrations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAssetRegistrations")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetRegistrations")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetRegistrations", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetRegistration}", name = "add-fixed-asset-registration", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetRegistration", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFixedAssetRegistrations {}

    @Screen(name = "ListFixedAssetMaints", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListFixedAssetMaints")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssetMaints")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListFixedAssetMaints")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetMaints", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewFixedAssetMaint}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFixedAssetMaint"
                )}, position = 0)})
        }
    )
    public interface ListFixedAssetMaints {}

    @Screen(name = "EditFixedAssetMaint", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetMaintenance")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFixedAssetMaints")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetMaintenance")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "maintHistSeqId", fromField = "parameters.maintHistSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAssetMaint", valueField = "fixedAssetMaint")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"maintHistSeqId"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAddFixedAssetMaintenance")}))
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditFixedAssetMaint", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"maintHistSeqId"}
                ),
                @Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "CREATE"
            })}), widgets = @InlineWidgets(containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewFixedAssetMaint}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFixedAssetMaint"
                )})}), position = 0)})
        }
    )
    public interface EditFixedAssetMaint {}

    @Screen(name = "EditFixedAssetMeters", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetMeters")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAssetMeters")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetMaintenance")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "maintHistSeqId", fromField = "parameters.maintHistSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAssetMaint", valueField = "fixedAssetMaint")
    @DecoratorScreen(
        name = "CommonFixedAssetMaintDecorator",
        location = "component://assetmaint/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetMeters", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetMeter}", name = "add-fixedasset-meter", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetMeter", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFixedAssetMeters {}

    @Screen(name = "FixedAssetChildren", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListFixedAssetChildren")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FixedAssetChildren")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListFixedAssetChildren")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "trail", fromField = "parameters.trail", defaultValue = "${parameters.fixedAssetId}")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.trail")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_TREE, name = "TreeFixedAsset", location = "component://accounting/widget/ledger/AccountingTrees.xml"
            )})
        }
    )
    public interface FixedAssetChildren {}

    @Screen(name = "EditFixedAssetMaintOrders", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFixedAssetMaintOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFixedAssetMaintOrders")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditFixedAssetMaintenance")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "maintHistSeqId", fromField = "parameters.maintHistSeqId")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.orderId")
    @Action(type = ActionType.SET, field = "orderItemSeqId", fromField = "parameters.orderItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAssetMaint", valueField = "fixedAssetMaint")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAssetMaintOrder", valueField = "fixedAssetMaintOrder")
    @DecoratorScreen(
        name = "CommonFixedAssetMaintDecorator",
        location = "component://assetmaint/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFixedAssetMaintOrders", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetMaintOrder}", name = "add-fixedasset-maint-order", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFixedAssetMaintOrder", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFixedAssetMaintOrders {}

    @Screen(name = "EditPartyFixedAssetAssignments", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyFixedAssetAssignments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyFixedAssetAssignments")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditPartyFixedAssetAssignments")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "parameters.roleTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyFixedAssetAssignment", valueField = "partyFixedAssetAssignment")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyFixedAssetAssignments", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddFixedAssetPartyAssignment}", name = "add-party-fixedasset-assignments", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyFixedAssetAssignment", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyFixedAssetAssignments {}

    @Screen(name = "ShowFixedAssetDepreciation", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFixedAssetDepreciationReport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FixedAssetDepreciation")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFixedAssetDepreciationReport")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.SERVICE, serviceName = "calculateFixedAssetDepreciation", resultMapName = "assetDepreciationResultMap", fieldMaps = {@FieldMap(fieldName = "fixedAssetId", fromField = "parameters.fixedAssetId")})
    @Action(type = ActionType.SET, field = "assetDepreciationInfoList", fromField = "assetDepreciationResultMap.assetDepreciationInfoList")
    @Action(type = ActionType.SET, field = "assetDepreciationResultMessages", fromField = "assetDepreciationResultMap.successMessageList")
    @Action(type = ActionType.SET, field = "depreciation", fromField = "fixedAsset.depreciation", valueType = "BigDecimal", defaultValue = "0.0")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleFixedAssetDepreciationHistory}", labels = {
                    @Label(text = "${uiLabelMap.FormFieldTitle_purchaseCost}: ${fixedAsset.purchaseCost}"
                ),
                @Label(text = "${uiLabelMap.FormFieldTitle_depreciation}: ${depreciation}"
            ),
            @Label(text = "${uiLabelMap.FormFieldTitle_salvageValue}: ${fixedAsset.salvageValue}"
            ),
            @Label(text = "${uiLabelMap.FormFieldTitle_dateAcquired}: ${fixedAsset.dateAcquired}"
            ),
            @Label(text = "${uiLabelMap.FormFieldTitle_expectedEndOfLife}: ${fixedAsset.expectedEndOfLife}"
            ),
            @Label(text = "${uiLabelMap.FormFieldTitle_NextDepreciationAmount}: ${assetDepreciationResultMap.nextDepreciationAmount}"
            )}, sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"assetDepreciationResultMap.plannedPastDepreciationTotal"
                }),
                @Condition(type = NotEmpty.class, params = {"fixedAsset.partyId"
            }),
            @Condition(type = CompareField.class, params = {"assetDepreciationResultMap.plannedPastDepreciationTotal", "greater", "depreciation", "BigDecimal"
            })}), actions = @Actions(value = {
                @Action(type = ActionType.SERVICE, serviceName = "getGlAccountFromAccountType", resultMapName = "defaultCreditAccountForDepreciationTransactionResult", fieldMaps = {
                    @FieldMap(fieldName = "organizationPartyId", fromField = "fixedAsset.partyId"
                ),
                @FieldMap(fieldName = "acctgTransTypeId", value = "DEPRECIATION"
            ),
            @FieldMap(fieldName = "fixedAssetId", fromField = "fixedAsset.fixedAssetId"
            ),
            @FieldMap(fieldName = "debitCreditFlag", value = "C")}),
            @Action(type = ActionType.SET, field = "defaultCreditAccountForDepreciationTransaction", fromField = "defaultCreditAccountForDepreciationTransactionResult.glAccountId", defaultValue = " "
            ),
            @Action(type = ActionType.SERVICE, serviceName = "getGlAccountFromAccountType", resultMapName = "defaultDebitAccountForDepreciationTransactionResult", fieldMaps = {
                @FieldMap(fieldName = "organizationPartyId", fromField = "fixedAsset.partyId"
            ),
            @FieldMap(fieldName = "acctgTransTypeId", value = "DEPRECIATION"
            ),
            @FieldMap(fieldName = "fixedAssetId", fromField = "fixedAsset.fixedAssetId"
            ),
            @FieldMap(fieldName = "debitCreditFlag", value = "D")}),
            @Action(type = ActionType.SET, field = "defaultDebitAccountForDepreciationTransaction", fromField = "defaultDebitAccountForDepreciationTransactionResult.glAccountId", defaultValue = " "
            )}), widgets = @WidgetsForContainer(containers = {
                @Container2(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingCreateAnAccountingTransaction}: ${assetDepreciationResultMap.plannedPastDepreciationTotal}", style = "${styles.link_nav_long} ${styles.action_add}", target = "CreateAcctgTransAndEntries"
                )})}))}, position = 0),
                @Screenlet(title = "${uiLabelMap.PageTitleFixedAssetDepreciationMethod}", includeForms = {
                    @IncludeForm(name = "AddFixedAssetDepMethod", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                ),
                @IncludeForm(name = "ListFixedAssetDepMethods", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, position = 1),
            @Screenlet(title = "${uiLabelMap.AccountingGlMappings}", includeForms = {
                @IncludeForm(name = "AddFixedAssetTypeGlAccount", location = "component://accounting/widget/assets/FixedAssetForms.xml", position = 0
            ),
            @IncludeForm(name = "FixedAssetTypeGlAccounts", location = "component://accounting/widget/assets/FixedAssetForms.xml", position = 2
            ),
            @IncludeForm(name = "GlobalFixedAssetTypeGlAccounts", location = "component://accounting/widget/assets/FixedAssetForms.xml", position = 4
            )}, labels = {
                @Label(text = "${uiLabelMap.PageTitleFixedAssetMappings}", position = 1
            ),
            @Label(text = "${uiLabelMap.PageTitleFixedAssetGlobalMappings}", position = 3
            )}, position = 2),
            @Screenlet(title = "${uiLabelMap.AccountingTransactions}", includeForms = {
                @IncludeForm(name = "FixedAssetTransactions", location = "component://accounting/widget/assets/FixedAssetForms.xml"
            )}, position = 4)}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"assetDepreciationInfoList"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleFixedAssetDepreciationReport}", includeForms = {
                        @IncludeForm(name = "ListFixedAssetDepreciations", location = "component://accounting/widget/assets/FixedAssetForms.xml"
                    )})}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleFixedAssetDepreciationReport}", labels = {
                            @Label(text = "${assetDepreciationResultMessages}")})}), position = 3
                        )})
        }
    )
    public interface ShowFixedAssetDepreciation {}

    @Screen(name = "FixedAssetGeoLocation", location = "component://accounting/widget/assets/FixedAssetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFixedAssetGeoLocation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FixedAssetGeoLocation")
    @Action(type = ActionType.SET, field = "fixedAssetId", fromField = "parameters.fixedAssetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAsset", valueField = "fixedAsset")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/fixedasset/FixedAssetGeoLocation.groovy")
    @DecoratorScreen(
        name = "FixedAssetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLatitude} ${latestGeoPoint.latitude}"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLongitude} ${latestGeoPoint.longitude}"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonElevation} ${latestGeoPoint.elevation} ${elevationUomAbbr}"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface FixedAssetGeoLocation {}

}
