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
public class CustomerScreens {

    @Screen(name = "customerBasicFields", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "fieldNamePrefix", fromField = "cbfFieldNamePrefix", defaultValue = "${''}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/customerbasicfields.ftl")}))
    public interface customerBasicFields {}

    @Screen(name = "eftAccountFields", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "fieldNamePrefix", fromField = "eafFieldNamePrefix", defaultValue = "${''}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/eftaccountfields.ftl")}))
    public interface eftAccountFields {}

    @Screen(name = "creditCardFields", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "ccfTemplateLocation", value = "component://shop/webapp/shop/customer/creditcardfields.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "creditCardFields", location = "component://accounting/widget/CommonScreens.xml")}))
    public interface creditCardFields {}

    @Screen(name = "billaddresspickfields", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "pickFieldClass", fromField = "bapfPickFieldClass", defaultValue = "bill-addr-pick-field")
    @Action(type = ActionType.SET, field = "fieldNamePrefix", fromField = "bapfFieldNamePrefix", defaultValue = "${''}")
    @Action(type = ActionType.SET, field = "fieldIdPrefix", fromField = "bapfFieldIdPrefix", defaultValue = "${fieldNamePrefix}")
    @Action(type = ActionType.SET, field = "newAddrFieldNamePrefix", fromField = "bapfNewAddrFieldNamePrefix", defaultValue = "${fieldNamePrefix}")
    @Action(type = ActionType.SET, field = "newAddrFieldIdPrefix", fromField = "bapfNewAddrFieldIdPrefix", defaultValue = "${newAddrFieldNamePrefix}")
    @Action(type = ActionType.SET, field = "useNewAddr", fromField = "bapfUseNewAddr", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "useUpdate", fromField = "bapfUseUpdate", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "updateLink", fromField = "bapfUpdateLink", valueType = "String")
    @Action(type = ActionType.SET, field = "donePage", fromField = "bapfDonePage", valueType = "String")
    @Action(type = ActionType.SET, field = "newAddrInline", fromField = "bapfNewAddrInline", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "newAddrContentId", fromField = "bapfNewAddrContentId", defaultValue = "newbilladdrcontent")
    @Action(type = ActionType.SET, field = "newAddrFieldId", fromField = "bapfNewAddrFieldId", defaultValue = "newbilladdrfield")
    @Action(type = ActionType.SET, field = "useScripts", fromField = "bapfUseScripts", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "showVerbose", fromField = "bapfShowVerbose", valueType = "Boolean", defaultValue = "false")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"editPaymentMethodDataPrepared"})}), widgets = @Widgets(sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditPaymentMethod.groovy")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/billaddresspickfields.ftl")}))}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/billaddresspickfields.ftl")}))
    public interface billaddresspickfields {}

    @Screen(name = "postalAddressFields", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "fieldNamePrefix", fromField = "pafFieldNamePrefix", defaultValue = "${''}")
    @Action(type = ActionType.SET, field = "fieldIdPrefix", fromField = "pafFieldIdPrefix", defaultValue = "${fieldNamePrefix}")
    @Action(type = ActionType.SET, field = "useScripts", fromField = "pafUseScripts", valueType = "Boolean", defaultValue = "true")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/postaladdressfields.ftl")}))
    public interface postalAddressFields {}

    @Screen(name = "editcontactmech", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyEditContactInfo")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditContactMech.groovy")
    @Action(type = ActionType.SET, field = "reqName", fromField = "requestName")
    @Action(type = ActionType.SET, field = "dependentForm", value = "editcontactmechform")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", fromField = "selectedStateName", defaultValue = "_none_")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
                    ),
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/editcontactmech.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface editcontactmech {}

    @Screen(name = "editcreditcard", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCreditCard")
    @Action(type = ActionType.SET, field = "cardNumberMinDisplay", value = "min")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditPaymentMethod.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/editcreditcard.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface editcreditcard {}

    @Screen(name = "editeftaccount", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditEFTAccount")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditPaymentMethod.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/editeftaccount.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface editeftaccount {}

    @Screen(name = "editgiftcard", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditGiftCard")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditPaymentMethod.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/editgiftcard.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface editgiftcard {}

    @Screen(name = "changepassword", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleChangePassword")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/ChangePassword.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifTrue = {"userHasAccount", "hasVerifyHash"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/changepassword.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface changepassword {}

    @Screen(name = "editperson", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyEditPersonalInformation")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "userLogin", relationName = "Person", toValueField = "person")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditPerson.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/editperson.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface editperson {}

    @Screen(name = "giftcardbalance", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleGiftCardBalance")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/GiftCardBalance.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/giftcardbalance.ftl"
            )})
        }
    )
    public interface giftcardbalance {}

    @Screen(name = "giftcardlink", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleGiftCardLink")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/GiftCardLink.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/giftcardlink.ftl"
            )})
        }
    )
    public interface giftcardlink {}

    @Screen(name = "customersurvey", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProfileSurvey")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/CustomerSurvey.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/customersurvey.ftl"
            )})
        }
    )
    public interface customersurvey {}

    @Screen(name = "contactus", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogin")
    @Action(type = ActionType.SET, field = "pageHeader", value = "${uiLabelMap.CommonContactUs}")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "contactus")
    @Action(type = ActionType.SET, field = "submitRequest", value = "contactsubmit")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/newmsg.ftl"
            )})
        }
    )
    public interface contactus {}

    @Screen(name = "messagelist", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleMessageList")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "messagelist-include"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface messagelist {}

    @Screen(name = "messagelist-include", location = "component://shop/widget/CustomerScreens.xml")
    @Action(order = 0, type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEvent", list = "receivedCommunicationEvents", conditions = {@ConditionExpr(fieldName = "partyIdTo", operator = "equals", fromField = "userLogin.partyId")}, orderBy = {"-entryDate"})
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.showSent", "equals", "true"})}), then = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEvent", list = "sentCommunicationEvents", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "userLogin.partyId")}, orderBy = {"-entryDate"})}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/messagelist.ftl")}))
    public interface messagelist_include {}

    @Screen(name = "messagedetail", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleMessageDetail")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/messagedetail.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface messagedetail {}

    @Screen(name = "messagecreate", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNewMessage")
    @Action(type = ActionType.SET, field = "pageHeader", value = "${uiLabelMap.PageTitleNewMessage}")
    @Action(type = ActionType.SET, field = "showMessageLinks", value = "true")
    @Action(type = ActionType.SET, field = "submitRequest", value = "sendmessage")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "communicationEvent.partyIdFrom")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/newmsg.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface messagecreate {}

    @Screen(name = "digitalproductlist", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDigitalProductList")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProduct", list = "supplierProductList", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "userLogin.partyId")})
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"productStore.enableDigProdUpload", "equals", "Y"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/digitalproductlist.ftl"
                        )}), failWidgets = @WidgetsForContainer(containers = {
                            @Container2(labels = {
                                @Label(text = "${uiLabelMap.EcommerceSorryDigitalProductUploadNotEnabled}", style = "head2"
                            )})}))}), failWidgets = @InlineWidgets(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                            )}))})
        }
    )
    public interface digitalproductlist {}

    @Screen(name = "digitalproductedit", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDigitalProductEdit")
    @Action(type = ActionType.SET, field = "parameters.minimumOrderQuantity", value = "1", valueType = "BigDecimal")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SupplierProduct", valueField = "supplierProduct", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "userLogin.partyId"), @FieldMap(fieldName = "productId", fromField = "parameters.productId"), @FieldMap(fieldName = "currencyUomId", fromField = "parameters.currencyUomId"), @FieldMap(fieldName = "minimumOrderQuantity", fromField = "parameters.minimumOrderQuantity"), @FieldMap(fieldName = "availableFromDate", fromField = "parameters.availableFromDate")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductPrice", list = "productPriceList", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId"), @FieldMap(fieldName = "productPriceTypeId", value = "DEFAULT_PRICE"), @FieldMap(fieldName = "productPricePurposeId", value = "PURCHASE"), @FieldMap(fieldName = "productStoreGroupId", value = "_NA_")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductContentAndInfo", list = "productContentAndInfoList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId"), @ConditionExpr(fieldName = "productContentTypeId", value = "DIGITAL_DOWNLOAD")}, orderBy = {"contentId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAndRole", list = "ownerContentAndRoleList", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "userLogin.partyId"), @FieldMap(fieldName = "roleTypeId", value = "OWNER")}, orderBy = {"contentId"})
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"productStore.enableDigProdUpload", "equals", "Y"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/digitalproductedit.ftl"
                        )}), failWidgets = @WidgetsForContainer(containers = {
                            @Container2(labels = {
                                @Label(text = "${uiLabelMap.EcommerceSorryDigitalProductUploadNotEnabled}", style = "common-msg-info-important"
                            )})}))}), failWidgets = @InlineWidgets(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                            )}))})
        }
    )
    public interface digitalproductedit {}

    @Screen(name = "FinAccountList-include", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccount", list = "ownedFinAccountList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "ownerPartyId", operator = "equals", fromField = "userLogin.partyId"), @ConditionExpr(fieldName = "organizationPartyId", operator = "equals", fromField = "productStore.payToPartyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"ownedFinAccountList"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.EcommerceMyAccount}", name = "fin-account-list", widgets = {@Widget(type = WidgetType.ITERATE_SECTION, list = "ownedFinAccountList", entry = "ownedFinAccount", name = "FinAccountList-include-iterate1", location = "component://shop/widget/CustomerScreens.xml")})}))
    public interface FinAccountList_include {}

    @Screen(name = "FinAccountList-include-iterate1", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountTrans", list = "ownedFinAccountTransList", conditions = {@ConditionExpr(fieldName = "finAccountId", fromField = "ownedFinAccount.finAccountId")}, orderBy = {"transactionDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FinAccountAuth", list = "ownedFinAccountAuthList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "finAccountId", fromField = "ownedFinAccount.finAccountId")}, orderBy = {"authorizationDate"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "finAccountStatusItem", fieldMaps = {@FieldMap(fieldName = "statusId", fromField = "ownedFinAccount.statusId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Uom", valueField = "accountCurrencyUom", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "uomId", fromField = "ownedFinAccount.currencyUomId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/FinAccountDetail.ftl")}))
    public interface FinAccountList_include_iterate1 {}

    @Screen(name = "SerializedInventorySummary", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItem", list = "inventoryItemList", conditions = {@ConditionExpr(fieldName = "inventoryItemTypeId", operator = "equals", value = "SERIALIZED_INV_ITEM"), @ConditionExpr(fieldName = "ownerPartyId", operator = "equals", fromField = "userLogin.partyId")}, orderBy = {"-createdStamp"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"inventoryItemList"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/SerializedInventorySummary.ftl")}))
    public interface SerializedInventorySummary {}

    @Screen(name = "SubscriptionSummary", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Subscription", list = "subscriptionList", filterByDate = true, conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "userLogin.partyId")}, orderBy = {"-fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"subscriptionList"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/SubscriptionSummary.ftl")}))
    public interface SubscriptionSummary {}

    @Screen(name = "newcustomer", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceRegister")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/NewCustomer.groovy")
    @Action(type = ActionType.SET, field = "dependentForm", value = "newuserform")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", defaultValue = "_previous_")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/newcustomer.ftl"
            )})
        }
    )
    public interface newcustomer {}

    @Screen(name = "viewprofile", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewProfile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "person")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "partyGroup")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/ViewProfile.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/viewprofile.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface viewprofile {}

    @Screen(name = "EditProfile", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceEditProfile")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/shop/images/profile.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditShippingAddress.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditEmailAndTelecomNumber.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/profile/EditProfile.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface EditProfile {}

    @Screen(name = "ManageAddress", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceManageAddresses")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/shop/images/profile.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditShippingAddress.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditBillingAddress.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/ViewProfile.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/profile/ManageAddress.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ManageAddress {}

    @Screen(name = "AnonContactus", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogin")
    @Action(type = ActionType.SET, field = "pageHeader", value = "${uiLabelMap.CommonContactUs}")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "contactus")
    @Action(type = ActionType.SET, field = "submitRequest", value = "contactsubmit")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/generated/AnonContactus_script1.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/AnonContactus.ftl"
            )})
        }
    )
    public interface AnonContactus {}

    @Screen(name = "showProductReviews", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/CustomerReviews.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"reviews"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/customer/viewreviews.ftl")}))
    public interface showProductReviews {}

    @Screen(name = "OptOutResponse", location = "component://shop/widget/CustomerScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "optOutOfListFromCommEvent", resultMapName = "optOutResult")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList", fieldMaps = {@FieldMap(fieldName = "contactListId", fromField = "optOutResult.contactListId")})
    @Action(type = ActionType.SET, field = "contactListId", fromField = "contactList.contactListId")
    @Action(type = ActionType.SET, field = "screenName", fromField = "contactList.optOutScreen", defaultValue = "component://shop/widget/CustomerScreens.xml#DefaultOptOutScreen")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${screenName}", shareScope = true)}))
    public interface OptOutResponse {}

    @Screen(name = "DefaultOptOutScreen", location = "component://shop/widget/CustomerScreens.xml")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "Opt-Out Results", containers = {
                    @Container(labels = {
                        @Label(text = "You have been successfully removed from the ${contactList.contactListName} mailing list!"
                    )})})})
        }
    )
    public interface DefaultOptOutScreen {}

}
