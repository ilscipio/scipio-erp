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
public class SettingsPaymentGatewayConfigScreens {

    @Screen(name = "FindPaymentGatewayConfig", location = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "paymentGatewayConfigTab")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPaymentGatewayConfig", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentGatewayConfig", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                    )}))})})
        }
    )
    public interface FindPaymentGatewayConfig {}

    @Screen(name = "EditPaymentGatewayConfig", location = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUpdatePaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "paymentGatewayConfigTab")
    @Action(type = ActionType.SET, field = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayConfig", valueField = "paymentGatewayConfig")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewaySagePay", valueField = "paymentGatewaySagePay", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayAuthorizeNet", valueField = "paymentGatewayAuthorizeNet", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayCyberSource", valueField = "paymentGatewayCyberSource", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayEway", valueField = "paymentGatewayEway", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayPayflowPro", valueField = "paymentGatewayPayflowPro", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayPayPal", valueField = "paymentGatewayPayPal", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayClearCommerce", valueField = "paymentGatewayClearCommerce", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayWorldPay", valueField = "paymentGatewayWorldPay", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewaySecurePay", valueField = "paymentGatewaySecurePay", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayiDEAL", valueField = "paymentGatewayiDEAL", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayOrbital", valueField = "paymentGatewayOrbital", fieldMaps = {@FieldMap(fieldName = "paymentGatewayConfigId", fromField = "parameters.paymentGatewayConfigId")})
    @Action(type = ActionType.SCRIPT, location = "component://accounting/script/com/ilscipio/scipio/accounting/settings/PaymentGatewayConfig.groovy")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "SAGEPAY_CONFIG"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigSagePay}", includeForms = {
                        @IncludeForm(name = "EditPaymentGatewayConfigSagePay", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                    )})})),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "AUTHORIZE_NET_CONFIG"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigAuthorizeNet}", includeForms = {
                            @IncludeForm(name = "EditPaymentGatewayConfigAuthorizeNet", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                        )})})),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "CYBERSOURCE_CONFIG"
                        })}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigCyberSource}", includeForms = {
                                @IncludeForm(name = "EditPaymentGatewayConfigCyberSource", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                            )})})),
                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "PAYFLOWPRO_CONFIG"
                            })}), widgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigPayflowPro}", includeForms = {
                                    @IncludeForm(name = "EditPaymentGatewayConfigPayflowPro", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                )})})),
                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "PAYPAL_CONFIG"
                                })}), widgets = @InlineWidgets(screenlets = {
                                    @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigPayPal}", includeForms = {
                                        @IncludeForm(name = "EditPaymentGatewayConfigPayPal", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                    )})})),
                                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                        @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "CLEARCOMMERCE_CONFIG"
                                    })}), widgets = @InlineWidgets(screenlets = {
                                        @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigClearCommerce}", includeForms = {
                                            @IncludeForm(name = "EditPaymentGatewayConfigClearCommerce", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                        )})})),
                                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "WORLDPAY_CONFIG"
                                        })}), widgets = @InlineWidgets(screenlets = {
                                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigWorldPay}", includeForms = {
                                                @IncludeForm(name = "EditPaymentGatewayConfigWorldPay", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                            )})})),
                                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "SECUREPAY_CONFIG"
                                            })}), widgets = @InlineWidgets(screenlets = {
                                                @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigSecurePay}", includeForms = {
                                                    @IncludeForm(name = "EditPaymentGatewayConfigSecurePay", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                                )})})),
                                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                    @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "EWAY_CONFIG"
                                                })}), widgets = @InlineWidgets(screenlets = {
                                                    @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigEway}", includeForms = {
                                                        @IncludeForm(name = "EditPaymentGatewayConfigEway", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                                    )})})),
                                                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                        @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "IDEAL_CONFIG"
                                                    })}), widgets = @InlineWidgets(screenlets = {
                                                        @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigiDEAL}", includeForms = {
                                                            @IncludeForm(name = "EditPaymentGatewayConfigiDEAL", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                                        )})})),
                                                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "ORBITAL_CONFIG"
                                                        })}), widgets = @InlineWidgets(screenlets = {
                                                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigOrbital}", includeForms = {
                                                                @IncludeForm(name = "EditPaymentGatewayConfigOrbital", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                                                            )})})),
                                                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                                @Condition(type = NotEmpty.class, params = {"paymentGatewayPayPalRestModelEntity"
                                                            }),
                                                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "PAYPAL_REST_CFG"
                                                        })}), widgets = @InlineWidgets(screenlets = {
                                                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigPayPalRest}", includeForms = {
                                                                @IncludeForm(name = "EditPaymentGatewayPayPalRest", location = "component://paypal/widget/settings/PaymentGatewayConfigForms.xml"
                                                            )})})),
                                                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                                @Condition(type = NotEmpty.class, params = {"paymentGatewayStripeRestModelEntity"
                                                            }),
                                                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "STRIPE_REST_CFG"
                                                        })}), widgets = @InlineWidgets(screenlets = {
                                                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigStripeRest}", includeForms = {
                                                                @IncludeForm(name = "EditPaymentGatewayStripeRest", location = "component://stripe/widget/settings/PaymentGatewayConfigForms.xml"
                                                            )})})),
                                                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                                @Condition(type = NotEmpty.class, params = {"paymentGatewayRedsysModelEntity"
                                                            }),
                                                            @Condition(type = Compare.class, params = {"parameters.paymentGatewayConfigId", "equals", "REDSYS_CFG"
                                                        })}), widgets = @InlineWidgets(screenlets = {
                                                            @Screenlet(title = "${uiLabelMap.PageTitleUpdatePaymentGatewayConfigRedsys}", includeForms = {
                                                                @IncludeForm(name = "EditPaymentGatewayRedsys", location = "component://redsys/widget/settings/PaymentGatewayConfigForms.xml"
                                                            )})}))})
        }
    )
    public interface EditPaymentGatewayConfig {}

    @Screen(name = "FindPaymentGatewayConfigTypes", location = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPaymentGatewayConfigTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "paymentGatewayConfigTypesTab")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPaymentGatewayConfigTypes", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPaymentGatewayConfigTypes", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                    )}))})})
        }
    )
    public interface FindPaymentGatewayConfigTypes {}

    @Screen(name = "EditPaymentGatewayConfigType", location = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUpdatePaymentGatewayConfigType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PaymentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "paymentGatewayConfigTypesTab")
    @Action(type = ActionType.SET, field = "paymentGatewayConfigTypeId", fromField = "parameters.paymentGatewayConfigTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PaymentGatewayConfigType", valueField = "paymentGatewayConfigType")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPaymentGatewayConfigType", location = "component://accounting/widget/settings/PaymentGatewayConfigForms.xml"
                )})})
        }
    )
    public interface EditPaymentGatewayConfigType {}

}
