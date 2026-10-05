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
public class ContractsAgreementScreens {

    @Screen(name = "FindAgreement", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindAgreement")
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewAgreement}", style = "${styles.link_nav} ${styles.action_add}", target = "EditAgreement"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindAgreements", location = "component://accounting/widget/contracts/AgreementForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreements", location = "component://accounting/widget/contracts/AgreementForms.xml"
                        )}))})})
        }
    )
    public interface FindAgreement {}

    @Screen(name = "EditAgreement", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "agreements")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementItem", list = "agreementItems", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId")})
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.agreement ? 'PageTitleEditAgreement' : 'AccountingNewAgreement'}")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"agreement"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementItems", location = "component://accounting/widget/contracts/AgreementForms.xml", position = 2
                    ),
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementTerms", location = "component://accounting/widget/contracts/AgreementForms.xml", position = 3
                )}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditAgreement", location = "component://accounting/widget/contracts/AgreementForms.xml"
                    )}, position = 1)}, containers = {
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", htmlTemplates = {
                                @HtmlTemplate(location = "component://accounting/webapp/accounting/agreement/CopyAgreement.ftl"
                            )})}, position = 0)}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(includeForms = {
                                    @IncludeForm(name = "EditAgreement", location = "component://accounting/widget/contracts/AgreementForms.xml"
                                )})}))})
        }
    )
    public interface EditAgreement {}

    @Screen(name = "ListAgreementItems", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementItems")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementItem", list = "agreementItems", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId")})
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementItems", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItems {}

    @Screen(name = "EditAgreementItem", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItem")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementItems")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "EditAgreementItem")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementItem", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementItem {}

    @Screen(name = "EditAgreementTerms", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementTerm")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementTerms")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "EditAgreementTerms")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementTerms", location = "component://accounting/widget/contracts/AgreementForms.xml"
            )}, screenlets = {
                @Screenlet(name = "AgreementTermPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddAgreementTerm", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditAgreementTerms {}

    @Screen(name = "ListAgreementPromoAppls", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementPromoAppls")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementPromoAppls")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementPromoAppls")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementPromoAppl", list = "agreementPromoAppls", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"sequenceNum"})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementPromoAppls", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementPromoAppls {}

    @Screen(name = "EditAgreementPromoAppl", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementPromoAppl")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementPromoAppls")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementPromoAppls")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.SET, field = "agreementTermId", fromField = "parameters.agreementTermId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementPromoAppl", valueField = "agreementPromoAppl")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementPromoAppl", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementPromoAppl {}

    @Screen(name = "ListAgreementItemTerms", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItemTerms")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemTerms")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemTerms")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementTerm", list = "agreementTerms", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementItemTerms", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemTerms {}

    @Screen(name = "EditAgreementItemTerm", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItemTerm")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemTerms")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemTerms")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "agreementTermId", fromField = "parameters.agreementTermId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementTerm", valueField = "agreementTerm")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementItemTerm", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementItemTerm {}

    @Screen(name = "ListAgreementItemProducts", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItemProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemProducts")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemProducts")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementProductAppl", list = "agreementProducts", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"productId"})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementItemProducts", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemProducts {}

    @Screen(name = "ListAgreementItemFacilities", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItemFacilities")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemWarehouses")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemFacilities")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementFacilityAppl", list = "agreementFacilities", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"facilityId"})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementItemFacilities", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemFacilities {}

    @Screen(name = "ListAgreementItemProductsReport", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPageTitleAgreementPriceList")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementProductAppl", list = "agreementProducts", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"productId"})
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAgreement}", includeForms = {
                    @IncludeForm(name = "ViewAgreementInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingAgreementItem}", includeForms = {
                    @IncludeForm(name = "ViewAgreementItemInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingAgreementItemProductsForReport}", includeForms = {
                    @IncludeForm(name = "ListAgreementItemProductsForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemProductsReport {}

    @Screen(name = "ListAgreementItemFacilitiesReport", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPageTitleAgreementPriceList")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementFacilityAppl", list = "agreementFacilities", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"facilityId"})
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAgreement}", includeForms = {
                    @IncludeForm(name = "ViewAgreementInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingAgreementItem}", includeForms = {
                    @IncludeForm(name = "ViewAgreementItemInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemFacilitiesReport {}

    @Screen(name = "EditAgreementItemProduct", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItemProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemProducts")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementProductAppl", valueField = "agreementProductAppl")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementItemProduct", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementItemProduct {}

    @Screen(name = "EditAgreementItemFacility", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItemFacility")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemWarehouses")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementFacilityAppl", valueField = "agreementFacilityAppl")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementItemFacility", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementItemFacility {}

    @Screen(name = "ListAgreementItemSupplierProducts", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItemProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemSupplierProducts")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemSupplierProducts")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProduct", list = "agreementProducts", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"productId"})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonPrint}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ListAgreementItemSupplierProductsReport", targetWindow = "_BLANK"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "ListAgreementItemSupplierProducts", location = "component://accounting/widget/contracts/AgreementForms.xml"
                    )}, position = 1)})
        }
    )
    public interface ListAgreementItemSupplierProducts {}

    @Screen(name = "ListAgreementItemSupplierProductsReport", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingPageTitleAgreementPriceList")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProduct", list = "agreementProducts", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")}, orderBy = {"productId"})
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAgreement}", includeForms = {
                    @IncludeForm(name = "ViewAgreementInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingAgreementItem}", includeForms = {
                    @IncludeForm(name = "ViewAgreementItemInfoForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingAgreementItemSupplierProductsForReport}", includeForms = {
                    @IncludeForm(name = "ListAgreementItemSupplierProductsForReport", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemSupplierProductsReport {}

    @Screen(name = "EditAgreementItemSupplierProduct", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItemProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementItems")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemSupplierProducts")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SupplierProduct", valueField = "agreementProductAppl")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"agreement.partyIdTo"}), @Condition(type = NotEmpty.class, params = {"agreementItem.currencyUomId"})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingAgreementIsNotSetForSupplierProducts}\n                                ", style = "heading"
            )})
        }
    )))
    public interface EditAgreementItemSupplierProduct {}

    @Screen(name = "ListAgreementItemParties", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementItemParties")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemParties")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemParties")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementPartyApplic", list = "agreementParties", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementItemParties", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementItemParties {}

    @Screen(name = "EditAgreementItemParty", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementItemParty")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementItemParties")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementItemParties")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementPartyApplic", valueField = "agreementPartyApplic")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementItemParty", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementItemParty {}

    @Screen(name = "ListAgreementGeographicalApplic", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListAgreementGeographicalApplic")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementGeographicalApplic")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementGeographicalApplic")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementGeographicalApplic", list = "agreementGeographicalApplics", fieldMaps = {@FieldMap(fieldName = "agreementId", fromField = "agreement.agreementId"), @FieldMap(fieldName = "agreementItemSeqId", fromField = "agreementItem.agreementItemSeqId")})
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListAgreementGeographicalApplic", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface ListAgreementGeographicalApplic {}

    @Screen(name = "EditAgreementGeographicalApplic", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementGeographicalApplic")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListAgreementGeographicalApplic")
    @Action(type = ActionType.SET, field = "buttonBarItem", value = "ListAgreementGeographicalApplic")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "geoId", fromField = "parameters.geoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementItem", valueField = "agreementItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "AgreementGeographicalApplic", valueField = "agreementGeographicalApplic")
    @DecoratorScreen(
        name = "AgreementSubDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAgreementGeographicalApplic", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )})})
        }
    )
    public interface EditAgreementGeographicalApplic {}

    @Screen(name = "EditAgreementWorkEffortApplics", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementWorkEffortApplics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementWorkEffort")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementWorkEffortApplics", location = "component://accounting/widget/contracts/AgreementForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddAgreementWorkEffortApplic}", name = "AgreementWorkEffortApplicsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddAgreementWorkEffortApplic", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditAgreementWorkEffortApplics {}

    @Screen(name = "EditAgreementRoles", location = "component://accounting/widget/contracts/AgreementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindAgreementRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AgreementRoles")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Agreement", valueField = "agreement")
    @DecoratorScreen(
        name = "CommonAgreementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementRoles", location = "component://accounting/widget/contracts/AgreementForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddAgreementRoles}", name = "add-agreement-roles", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddAgreementRole", location = "component://accounting/widget/contracts/AgreementForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditAgreementRoles {}

}
