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
public class CatalogFeatureScreens {

    @Screen(name = "EditFeature", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeature")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFeatures")
    @Action(type = ActionType.SET, field = "productFeatureId", fromField = "parameters.productFeatureId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeature", valueField = "productFeature")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProductFeature", list = "supplierProductFeatures", fieldMaps = {@FieldMap(fieldName = "productFeatureId", fromField = "parameters.productFeatureId")})
    @Action(type = ActionType.SET, field = "isCreateFeature", value = "${groovy: !(context.productFeature || (parameters.productFeatureId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateFeature ? 'ProductNewFeature' : 'ProductFeature'}")
    @DecoratorScreen(
        name = "CommonSpecificFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"productFeature"})}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleEditFeaturePrice}", includeForms = {
                                @IncludeForm(name = "ListFeaturePrice", location = "component://product/widget/catalog/FeatureForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.PageTitleAddFeaturePrice}", includeForms = {
                                @IncludeForm(name = "CreateFeaturePrice", location = "component://product/widget/catalog/FeatureForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.ProductSupplierSpecificFeatureInformation}", includeForms = {
                                @IncludeForm(name = "EditSupplierProductFeatures", location = "component://product/widget/catalog/FeatureForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.ProductCreateInformationNewSupplier}", includeForms = {
                                @IncludeForm(name = "CreateSupplierProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                            )})}))})
        }
    )
    public interface EditFeature {}

    @Screen(name = "EditFeatureTypes", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeatureTypes")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "FeatureTypeSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFeatureType", location = "component://product/widget/catalog/FeatureForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFeatureTypes", location = "component://product/widget/catalog/FeatureForms.xml"
                    )}))})})
        }
    )
    public interface EditFeatureTypes {}

    @Screen(name = "EditFeatureType", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeatureType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeatureType")
    @Action(type = ActionType.SET, field = "productFeatureTypeId", fromField = "parameters.productFeatureTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeatureType", valueField = "productFeatureType")
    @Action(type = ActionType.SET, field = "isCreateFeatureType", value = "${groovy: !(context.productFeatureType || (parameters.productFeatureTypeId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateFeatureType ? 'ProductNewFeatureType' : 'ProductFeatureType' }")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.productFeatureTypeId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FeatureTypeSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditFeatureType", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface EditFeatureType {}

    @Screen(name = "EditFeatureInterActions", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureInterActions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureInterAction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeatureInterActions")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "FeatureInterActionSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFeatureInterAction", location = "component://product/widget/catalog/FeatureForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFeatureInterActions", location = "component://product/widget/catalog/FeatureForms.xml"
                    )}))})})
        }
    )
    public interface EditFeatureInterActions {}

    @Screen(name = "EditFeatureInterAction", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeatureInterAction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureInterAction")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeatureInterAction")
    @Action(type = ActionType.SET, field = "productFeatureId", fromField = "parameters.productFeatureId")
    @Action(type = ActionType.SET, field = "productFeatureIdTo", fromField = "parameters.productFeatureIdTo")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeatureIactn", valueField = "productFeatureIactn")
    @Action(type = ActionType.SET, field = "isCreateFeatureInterAction", value = "${groovy: !(context.productFeatureIactn || (parameters.productFeatureId && parameters.productFeatureIdTo && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateFeatureInterAction ? 'ProductNewFeatureInterAction' : 'ProductFeatureInterAction'}")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.productFeatureTypeId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FeatureInterActionSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditFeatureInterAction", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface EditFeatureInterAction {}

    @Screen(name = "CreateProductFeature", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductNewFeatureCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureCategory")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CreateProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface CreateProductFeature {}

    @Screen(name = "EditFeatureCategories", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureCategory")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureCategories")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX_1", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE_1", valueType = "Integer", defaultValue = "10")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewFeatureCategory}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateProductFeature"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProductFeatureCategory", location = "component://product/widget/catalog/FeatureForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductFeatureCategory", location = "component://product/widget/catalog/FeatureForms.xml"
                        )}))})})
        }
    )
    public interface EditFeatureCategories {}

    @Screen(name = "EditFeatureCategoryFeatures", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureCategoryFeatures")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureCategory")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/feature/EditFeatureCategoryFeatures.groovy")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/feature/EditFeatureCategoryFeatures.ftl"
            )})
        }
    )
    public interface EditFeatureCategoryFeatures {}

    @Screen(name = "EditFeatureGroups", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureGroup")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/feature/EditFeatureGroups.groovy")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewGroup}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductFeatureGroup"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFeatureGroup", location = "component://product/widget/catalog/FeatureForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/feature/EditFeatureGroups.ftl"
                        )}))})})
        }
    )
    public interface EditFeatureGroups {}

    @Screen(name = "EditFeatureGroup", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureGroup")
    @Action(type = ActionType.SET, field = "productFeatureGroupId", fromField = "parameters.productFeatureGroupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeatureGroup", valueField = "productFeatureGroup")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "CreateFeatureGroup", location = "component://product/widget/catalog/FeatureForms.xml"
            )})
        }
    )
    public interface EditFeatureGroup {}

    @Screen(name = "EditFeatureGroupAppls", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFeatureGroupAppls")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureGroup")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "productFeatureGroupId", fromField = "parameters.productFeatureGroupId")
    @Action(type = ActionType.SET, field = "productFeatureCategoryId", fromField = "parameters.productFeatureCategoryId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductFeatureGroupAndAppl", list = "productFeatureGroupAndAppls", fieldMaps = {@FieldMap(fieldName = "productFeatureGroupId")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductFeatureCategory", list = "productFeatureCategories", orderBy = {"description"})
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductFeature", list = "productFeatures", fieldMaps = {@FieldMap(fieldName = "productFeatureCategoryId")})
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductEditFeatureGroupAppls}", includeForms = {
                    @IncludeForm(name = "ListFeatureGroupAppls", location = "component://product/widget/catalog/FeatureForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductQuickApplyFeature}", includeForms = {
                    @IncludeForm(name = "QuickApplyFeatureToGroup", location = "component://product/widget/catalog/FeatureForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductApplyFeaturesFromCategory}", includeForms = {
                    @IncludeForm(name = "ApplyFeatureCategoryToGroup", location = "component://product/widget/catalog/FeatureForms.xml"
                ),
                @IncludeForm(name = "ApplyFeaturesFromCategoryToGroup", location = "component://product/widget/catalog/FeatureForms.xml"
            )})})
        }
    )
    public interface EditFeatureGroupAppls {}

    @Screen(name = "QuickAddProductFeatures", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductAddFeatureInBulk")
    @Action(type = ActionType.SET, field = "featureNum", fromField = "parameters.featureNum", valueType = "Integer")
    @Action(type = ActionType.SET, field = "productFeatureCategoryId", fromField = "parameters.productFeatureCategoryId")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FeatureCategory")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/feature/QuickAddProductFeatures.groovy")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/feature/BulkAddFeature.ftl"
            )})
        }
    )
    public interface QuickAddProductFeatures {}

    @Screen(name = "ListFeaturePrice", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeaturePrice")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "FormFieldTitle_featurePrice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Feature")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeaturePrice")
    @DecoratorScreen(
        name = "CommonSpecificFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditFeaturePrice}", includeForms = {
                    @IncludeForm(name = "ListFeaturePrice", location = "component://product/widget/catalog/FeatureForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddFeaturePrice}", includeForms = {
                    @IncludeForm(name = "CreateFeaturePrice", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface ListFeaturePrice {}

    @Screen(name = "CreateFeaturePrice", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "FormFieldTitle_featurePrice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Feature")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FeaturePrice")
    @DecoratorScreen(
        name = "CommonSpecificFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductQuickApplyFeature}", includeForms = {
                    @IncludeForm(name = "CreateFeaturePrice", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface CreateFeaturePrice {}

    @Screen(name = "CreateFeature", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeature")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductFeature")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFeatures")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeature", valueField = "productFeature")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProductFeature", list = "supplierProductFeatures", fieldMaps = {@FieldMap(fieldName = "productFeatureId", fromField = "parameters.productFeatureId")})
    @Action(type = ActionType.SET, field = "isCreateFeature", value = "${groovy: !(context.productFeature || (parameters.productFeatureId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateFeature ? 'ProductNewFeature' : 'ProductFeature'}")
    @DecoratorScreen(
        name = "CommonSpecificFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                )})})
        }
    )
    public interface CreateFeature {}

    @Screen(name = "ListFeatures", location = "component://product/widget/catalog/FeatureScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFeatures")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductFeature")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListFeatures")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX_1", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE_1", valueType = "Integer", defaultValue = "10")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewFeature}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateFeature"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductFeature", location = "component://product/widget/catalog/FeatureForms.xml"
                        )}))})})
        }
    )
    public interface ListFeatures {}

}
