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
package com.ilscipio.scipio.shop.widget;

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
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://shop/widget/CommonScreens.xml")
    public interface webapp_common_actions {}

    @Screen(name = "ShopActions", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "ShopUiLabelsActions")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "ShopSetupActions")
    public interface ShopActions {}

    @Screen(name = "ShopUiLabelsActions", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ComplianceUiLabels", mapName = "uiLabelMap", global = true)
    public interface ShopUiLabelsActions {}

    @Screen(name = "ShopSetupActions", location = "component://shop/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "layoutSettings.commonHeaderImageLinkUrl", fromField = "layoutSettings.commonHeaderImageLinkUrl", defaultValue = "main", global = true)
    @Action(order = 1, type = ActionType.SET, field = "layoutSettings.companyName", fromField = "layoutSettings.companyName", defaultValue = "SCIPIO Store", global = true)
    @Action(order = 2, type = ActionType.SET, field = "initialLocaleComplete", value = "${groovy:parameters?.userLogin?.lastLocale}", valueType = "String", defaultValue = "${groovy:locale.toString()}")
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"isEmptyJavascript"}, ifCompare = {@IfCompare(field = "isEmptyJavascript", operator = "not-equals", value = "Y")})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)}))
    @Action(order = 4, type = ActionType.ENTITY_AND, entityName = "WebAnalyticsConfig", list = "webAnalyticsConfigs", useCache = true, fieldMaps = {@FieldMap(fieldName = "webSiteId")})
    @Action(order = 5, type = ActionType.SET, field = "shopSetupScriptLocation", fromField = "shopSetupScriptLocation", defaultValue = "component://shop/webapp/shop/WEB-INF/actions/ShopSetup.groovy")
    @Action(order = 6, type = ActionType.SCRIPT, location = "${shopSetupScriptLocation}")
    @Action(order = 7, type = ActionType.SET, field = "permChecksSetGlobal", value = "true", valueType = "Boolean")
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy")
    @Action(order = 9, type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/compliance-static/css/compliance.css", global = true)
    public interface ShopSetupActions {}

    @Screen(name = "ShopDecorator", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "ShopActions")
    @DecoratorScreen(
        name = "GlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "pre-content", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "pre-content"
            )}),
            @DecoratorSection(name = "content-full-screen", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"customSideBar"})}), widgets = @InlineWidgets(containers = {
                        @Container(sections = {
                            @SectionNested(actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_0_main}"
                            )}), widgets = @WidgetsForContainer(containers = {
                                @Container2(id = "content-main-body", includeScreens = {
                                    @IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml"
                                )})}))})}),
                // SCIPIO: 4.0.0: the fail-widgets of the XML section (customSideBar false, as in ignite-shop) were lost in the
                // annotation migration; the decorator section still existed, so GlobalDecorator took the full-screen branch
                // and every shop page rendered an empty content-main-section. The four column layouts of the XML:
                failWidgets = @InlineWidgets(containers = {
                    @Container(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"left-column"}), @ConditionNode(type = False.class, params = {"showLeftColumn"})}),
                            @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"right-column"}), @ConditionNode(type = False.class, params = {"showRightColumn"})})}),
                            actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_0_main}")}),
                            widgets = @WidgetsForContainer(containers = {
                                @Container2(id = "content-main-body", includeScreens = {
                                    @IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml")})})),
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"left-column"}), @ConditionNode(not = true, type = False.class, params = {"showLeftColumn"})}),
                            @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"right-column"}), @ConditionNode(type = False.class, params = {"showRightColumn"})})}),
                            actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "columnLeftStyle", value = "${styles.grid_sidebar_1_side}"),
                                @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_1_main}")}),
                            widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-left", location = "component://common/widget/CommonScreens.xml"),
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main", location = "component://common/widget/CommonScreens.xml")})),
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"left-column"}), @ConditionNode(type = False.class, params = {"showLeftColumn"})}),
                            @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"right-column"}), @ConditionNode(not = true, type = False.class, params = {"showRightColumn"})})}),
                            actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "columnRightStyle", value = "${styles.grid_sidebar_1_side}"),
                                @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_1_main}")}),
                            widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main", location = "component://common/widget/CommonScreens.xml"),
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-right", location = "component://common/widget/CommonScreens.xml")})),
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"left-column"}), @ConditionNode(not = true, type = False.class, params = {"showLeftColumn"})}),
                            @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"right-column"}), @ConditionNode(not = true, type = False.class, params = {"showRightColumn"})})}),
                            actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "columnLeftStyle", value = "${styles.grid_sidebar_2_side}"),
                                @Action(type = ActionType.SET, field = "columnRightStyle", value = "${styles.grid_sidebar_2_side}"),
                                @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_2_main}")}),
                            widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-left", location = "component://common/widget/CommonScreens.xml"),
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main", location = "component://common/widget/CommonScreens.xml"),
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-right", location = "component://common/widget/CommonScreens.xml")}))})}))})
        }
    )
    public interface ShopDecorator {}

    @Screen(name = "main-decorator", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.VIEW_SIZE", fromField = "parameters.VIEW_SIZE", defaultValue = "12")
    @Action(type = ActionType.SET, field = "parameters.INDEX_SIZE", fromField = "parameters.INDEX_SIZE", defaultValue = "0")
    @DecoratorScreen(
        name = "ShopDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-content", containers = {
                @Container(id = "pre-content-section", style = "${styles.grid_theme_pre}", decoratorSectionIncludes = {
                    @DecoratorSectionInclude(name = "pre-content")})}),
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "leftbar", location = "component://shop/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "pre-body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "pre-body")}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"globalContext.productStore"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "A Product Store has not been defined for this shop.", style = "warning"
                )}))})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonShopAppDecorator", location = "component://shop/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonShopAppDecorator {}

    @Screen(name = "leftbar", location = "component://shop/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "sidedeepcategory", location = "component://shop/widget/CatalogScreens.xml")}))
    public interface leftbar {}

    @Screen(name = "rightbar", location = "component://shop/widget/CommonScreens.xml")
    public interface rightbar {}

    @Screen(name = "CommonEmptyDecorator", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "center")
    @DecoratorScreen(
        name = "ShopDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonEmptyDecorator {}

    @Screen(name = "language", location = "component://shop/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocalesCompact", location = "component://common/widget/CommonScreens.xml")}))
    public interface language {}

    // SCIPIO: 4.0.0: the home page of a storefront theme that brings its own (VT_SHOP_HOME, e.g. Aurora Shop)
    @Screen(name = "themeHome", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "main")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/Main.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/themehome.ftl"
            )})
        }
    )
    public interface themeHome {}

    @Screen(name = "main", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/Main.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Category.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/script/com/ilscipio/scipio/shop/misc/Newsletter.groovy")
    @Action(type = ActionType.SET, field = "title")
    @Action(type = ActionType.SET, field = "titleProperty")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-content", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioDashboardImage", location = "component://shop/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioDashboardSlider", location = "component://shop/widget/CommonScreens.xml"
            )}, sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "productCategoryId", value = "PROMOTIONS"
                ),
                @Action(type = ActionType.SET, field = "viewSize", value = "12"
            ),
            @Action(type = ActionType.SET, field = "viewIndex", value = "0"
            ),
            @Action(type = ActionType.SET, field = "viewCluster", value = "4"
            ),
            @Action(type = ActionType.SET, field = "viewScrollCluster", value = "4"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioDashboardProductSlider"
            )}), position = 0),
            @InlineSection(actions = @Actions(value = {
                @Action(type = ActionType.SET, field = "productCategoryId", value = "ELTRN-100"
            ),
            @Action(type = ActionType.SET, field = "viewSize", value = "6"
            ),
            @Action(type = ActionType.SET, field = "viewIndex", value = "0"
            ),
            @Action(type = ActionType.SET, field = "viewCluster", value = "5"
            ),
            @Action(type = ActionType.SET, field = "viewScrollCluster", value = "1"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioDashboardMiniProductSlider"
            )}), position = 2),
            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                @Condition(type = True.class, params = {"context.isMaileonComponentPresent"
            }),
            @Condition(type = NotEmpty.class, params = {"context.productStoreMaileon"
            })}), actions = @Actions(value = {
                @Action(type = ActionType.SET, field = "depFormFieldPrefix", value = "MAILEON_CONTACT_"
            ),
            @Action(type = ActionType.SET, field = "dependentForm", value = "MAILEON_CONTACT_FORM"
            ),
            @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "mainId", value = "COUNTRY_"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "STATE_"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", defaultValue = "_previous_"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://maileon/webapp/maileon/maileon-section.ftl"
            )}), position = 3)})
        }
    )
    public interface main {}

    @Screen(name = "login", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogin")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "login")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/Login.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/generated/login_script1.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingAddress")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://shop/webapp/shop/login.ftl"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", htmlTemplates = {
                            @HtmlTemplate(location = "component://shop/webapp/shop/order/startanoncheckout.ftl"
                        )})})})
        }
    )
    public interface login {}

    @Screen(name = "requirePasswordChange", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogin")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "login")
    @Action(type = ActionType.SET, field = "tmpUserLogin", value = "${groovy: request.getAttribute('tmpUserLogin')}")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/Login.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifTrue = {"userHasAccount"}, ifCompare = {
                        @IfCompare(field = "tmpUserLogin.requirePasswordChange", operator = "equals", value = "Y"
                    )})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/requirePasswordChange.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface requirePasswordChange {}

    @Screen(name = "policies", location = "component://shop/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.SUB_CONTENT, contentId = "WebStoreCONTENT", mapKey = "policies"
            ),
            @Widget(type = WidgetType.SUB_CONTENT, contentId = "WebStoreCONTENT", mapKey = "policies2"
            )})
        }
    )
    public interface policies {}

    @Screen(name = "license", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonLicense")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/license.ftl"
            )})
        }
    )
    public interface license {}

    // SCIPIO: 4.0.0: store legal texts (compliance component)
    @Screen(name = "legal", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://compliance/script/shop/LegalPage.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/legalpage.ftl"
            )})
        }
    )
    public interface legal {}

    // SCIPIO: 4.0.0: Your Privacy Choices (compliance component)
    @Screen(name = "privacyChoices", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ComplianceYourPrivacyChoices")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/privacychoices.ftl"
            )})
        }
    )
    public interface privacyChoices {}

    // SCIPIO: 4.0.0: EU withdrawal function (compliance component)
    @Screen(name = "withdraw", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ComplianceWithdrawHere")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/withdraw.ftl"
            )})
        }
    )
    public interface withdraw {}

    // SCIPIO: 4.0.0: marketplace seller page (compliance component)
    @Screen(name = "seller", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ComplianceSeller")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/sellerpage.ftl"
            )})
        }
    )
    public interface seller {}

    // SCIPIO: 4.0.0: compliance component
    @Screen(name = "privacyCenter", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CompliancePrivacyCenter")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/privacycenter.ftl"
            )})
        }
    )
    public interface privacyCenter {}

    // SCIPIO: 4.0.0: compliance component
    @Screen(name = "privacyRequest", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CompliancePrivacyRequest")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/privacyrequest.ftl"
            )})
        }
    )
    public interface privacyRequest {}

    // SCIPIO: 4.0.0: page after the account deletion (compliance component)
    @Screen(name = "privacyDeleted", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CompliancePrivacyRequest")
    @Action(type = ActionType.SET, field = "scpPrivacyDoneOverride", value = "deleted")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/shop/privacyrequest.ftl"
            )})
        }
    )
    public interface privacyDeleted {}

    @Screen(name = "ListLocalesCompact", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonChooseLanguage}")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/generated/ListLocalesCompact_script1.groovy")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocalesCompact", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListLocalesCompact {}

    @Screen(name = "ScipioDashboardImage", location = "component://shop/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/dashboard/dashboardImage.ftl")})}))
    public interface ScipioDashboardImage {}

    @Screen(name = "ScipioDashboardSlider", location = "component://shop/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/dashboard/slider.ftl")})}))
    public interface ScipioDashboardSlider {}

    @Screen(name = "ScipioDashboardProductSlider", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "productCategoryId", valueType = "String", defaultValue = "CATALOG1")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "viewSize", defaultValue = "12")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "viewIndex", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewCluster", fromField = "viewCluster", defaultValue = "4")
    @Action(type = ActionType.SET, field = "viewScrollCluster", fromField = "viewScrollCluster", defaultValue = "4")
    @Action(type = ActionType.SET, field = "localVarsOnly", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Category.groovy")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "localVarsOnly", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/CategoryDetail.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/dashboard/productSlider.ftl")})}))
    public interface ScipioDashboardProductSlider {}

    @Screen(name = "ScipioDashboardMiniProductSlider", location = "component://shop/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "productCategoryId", valueType = "String", defaultValue = "CATALOG1")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "viewSize", defaultValue = "12")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "viewIndex", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewCluster", fromField = "viewCluster", defaultValue = "4")
    @Action(type = ActionType.SET, field = "viewScrollCluster", fromField = "viewScrollCluster", defaultValue = "4")
    @Action(type = ActionType.SET, field = "localVarsOnly", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Category.groovy")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#miniproductsummary")
    @Action(type = ActionType.SET, field = "localVarsOnly", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/CategoryDetail.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/dashboard/productSlider.ftl")})}))
    public interface ScipioDashboardMiniProductSlider {}

}
