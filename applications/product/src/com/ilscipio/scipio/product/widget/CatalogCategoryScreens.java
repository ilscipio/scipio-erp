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
public class CatalogCategoryScreens {

    @Screen(name = "FindCategory", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindCategory")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindCategory")
    @Action(type = ActionType.SET, field = "isSpecificCategory", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindCategory", location = "component://product/widget/catalog/CategoryForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCategory", location = "component://product/widget/catalog/CategoryForms.xml"
                    )}))})})
        }
    )
    public interface FindCategory {}

    @Screen(name = "EditCategory", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategory")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryType", list = "productCategoryTypes", orderBy = {"description"})
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategory.groovy")
    @Action(type = ActionType.SET, field = "isCreateCategory", value = "${groovy: !(context.productCategory || (parameters.productCategoryId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateCategory ? 'ProductNewCategory' : 'ProductCategory'}")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/EditCategory.ftl"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"parameters.duplicateCategory"
                }),
                @Condition(type = Compare.class, params = {"parameters.duplicateCategory", "equals", "Y"
            })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/DuplicateCategory.ftl"
            )}))})
        }
    )
    public interface EditCategory {}

    @Screen(name = "EditCategorySection", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCategories")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategory")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategory")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryType", list = "productCategoryTypes", orderBy = {"description"})
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategory.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/EditCategory.ftl"
            )}, sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId"
                ),
                @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
            )}, sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"productCategory"})}
                ), widgets = @WidgetsForContainer(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "CategoryTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                ),
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CategorySubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${productCategory.categoryName} [${productCategoryId}]  ${${extraFunctionName}}", style = "heading"
            )}), failWidgets = @WidgetsForContainer(sections = {
                @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "EditCategory"
                })}), widgets = @WidgetsForContainer2(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "CategorySubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                )}))}), position = 0)}), position = 0)})
        }
    )
    public interface EditCategorySection {}

    @Screen(name = "EditCategoryContent", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategoryContent")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "productCategory", relationName = "ProductCategoryType", toValueField = "productCategoryType")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryType", list = "productCategoryTypes")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryContentType", list = "productCategoryContentTypeList", orderBy = {"description"})
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategoryContent.groovy")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategoryContent")
    @Action(type = ActionType.SET, field = "TabBarName", value = "CategoryContentSubTabBar")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/content/images/ScpContentCommon.js", global = true)
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/UploadCategoryImage.ftl"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditCategoryContent}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/CategoryContentList.ftl"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.ProductCategoryCreateNewCategoryContent}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/AddCategoryContent.ftl"
                )}, position = 1),
                @Screenlet(title = "${uiLabelMap.ProductAddContentCategory}", includeForms = {
                    @IncludeForm(name = "AddCategoryContentAssoc", location = "component://product/widget/catalog/CategoryForms.xml"
                )}, position = 2),
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/EditCategoryContent.ftl"
                )}, position = 3),
                @Screenlet(title = "${uiLabelMap.CommonUpdateLocalizedFields}", includeScreens = {
                    @IncludeScreen(name = "EditCategoryStcLocFields", location = "component://product/widget/catalog/CategoryScreens.xml"
                )}, position = 5),
                @Screenlet(title = "${uiLabelMap.ProductAddAdditionalImages}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/AddAdditionalImages.ftl"
                )}, position = 6)})
        }
    )
    public interface EditCategoryContent {}

    @Screen(name = "EditCategoryContentContent", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/product/WEB-INF/actions/generated/EditCategoryContentContent_script1.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategoryContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategoryContent")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategoryContentContent.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategorySEO.groovy")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditCategoryContent}", includeForms = {
                    @IncludeForm(name = "${contentFormName}", location = "component://product/widget/catalog/CategoryForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"content.contentId"}
                    )}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditAltLocaleSimpleTextContent"
                    )}, screenlets = {
                        @Screenlet(title = "${uiLabelMap.ProductListAssociatedContentInfos} (${uiLabelMap.CommonAll})", includeForms = {
                            @IncludeForm(name = "ListAssociatedContentInfos", location = "component://product/widget/catalog/ProductForms.xml"
                        )})}))})
        }
    )
    public interface EditCategoryContentContent {}

    @Screen(name = "EditAltLocaleSimpleTextContent", location = "component://product/widget/catalog/CategoryScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productCategoryContent"}), @Condition(type = NotEmpty.class, params = {"productCategoryId"}), @Condition(type = NotEmpty.class, params = {"contentId"}), @Condition(type = NotEmpty.class, params = {"content.contentId"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductCreateSimpleTextContentForAlternateLocale}", includeForms = {@IncludeForm(name = "ListSimpleTextContentForAlternateLocale", location = "component://product/widget/catalog/CategoryForms.xml"), @IncludeForm(name = "CreateSimpleTextContentForAlternateLocale", location = "component://product/widget/catalog/CategoryForms.xml")})}))
    public interface EditAltLocaleSimpleTextContent {}

    @Screen(name = "EditCategoryRollup", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategoryRollup")
    @Action(type = ActionType.SET, field = "originalProductCategoryId", value = "${parameters.originalProductCategoryId}", defaultValue = "${parameters.productCategoryId}", global = true)
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategoryAssociations")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.ProductAddCategoryParent}", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "productCategoryAssociationMode", value = "parent"
                    
                    ),
                    @Action(type = ActionType.SET, field = "addCategoryFormName", value = "AddCategoryParent"
                
                )}), widgets = @WidgetsLeaf(includeScreens = {
                    @IncludeScreen(name = "ScipioAddCategoryAssociation", location = "component://product/widget/catalog/CategoryScreens.xml"
                
            )}))})}),
            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                @ScreenletNested(title = "${uiLabelMap.ProductAddCategoryChild}", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "productCategoryAssociationMode", value = "child"
                    
            ),
                    @Action(type = ActionType.SET, field = "addCategoryFormName", value = "AddCategoryChild"
                
            )}), widgets = @WidgetsLeaf(includeScreens = {
                    @IncludeScreen(name = "ScipioAddCategoryAssociation", location = "component://product/widget/catalog/CategoryScreens.xml"
                
            )}))})})}),
            @Container(style = "${styles.grid_row}", containers = {
                @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", sections = {
                    @SectionNested2(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "productCategoryAssociationMode", value = "parent"
                    )}), widgets = @WidgetsForContainer2(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioCategoryAssociationList"
                    )}))}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", sections = {
                        @SectionNested2(actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "productCategoryAssociationMode", value = "child"
                        )}), widgets = @WidgetsForContainer2(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioCategoryAssociationList"
                        )}))})})})
        }
    )
    public interface EditCategoryRollup {}

    @Screen(name = "ScipioAddCategoryAssociation", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/category/AddCategoryAssociation.ftl")})}))
    public interface ScipioAddCategoryAssociation {}

    @Screen(name = "ScipioCategoryAssociationList", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"productCategoryAssociationMode", "equals", "parent"})}), actions = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryRollup", list = "productCategoryRollupList", conditions = {@ConditionExpr(fieldName = "productCategoryId", fromField = "originalProductCategoryId")}, orderBy = {"sequenceNum"}), @Action(type = ActionType.SET, field = "sectionTitle", value = "${uiLabelMap.ProductCategoryParentCategoryList}")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"productCategoryAssociationMode", "equals", "child"})}), actions = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryRollup", list = "productCategoryRollupList", conditions = {@ConditionExpr(fieldName = "parentProductCategoryId", fromField = "originalProductCategoryId")}, orderBy = {"sequenceNum"}), @Action(type = ActionType.SET, field = "sectionTitle", value = "${uiLabelMap.ProductCategoryChildCategoryList}")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productCategoryRollupList"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${sectionTitle}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/category/CategoryAssociationList.ftl")})}))
    public interface ScipioCategoryAssociationList {}

    @Screen(name = "EditCategoryProducts", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCategoryProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategoryProducts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProducts")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/EditCategoryProducts.groovy")
    @Action(type = ActionType.SET, field = "TabBarName", value = "CategoryProductSubTabBar")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategoryProduct")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/AddCategoryProduct.ftl"
                )}),
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/category/EditCategoryProducts.ftl"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"parameters.copyProductToCategory"
                    }),
                    @Condition(type = Compare.class, params = {"parameters.copyProductToCategory", "equals", "Y"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/MoveCategoryProduct.ftl"
                )})),
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"parameters.expireAllCategoryProducts"
                }),
                @Condition(type = Compare.class, params = {"parameters.expireAllCategoryProducts", "equals", "Y"
            })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/ExpireCategoryProduct.ftl"
            )})),
            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                @Condition(type = NotEmpty.class, params = {"parameters.removeExpiredCategoryProducts"
            }),
            @Condition(type = Compare.class, params = {"parameters.removeExpiredCategoryProducts", "equals", "Y"
            })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/RemoveCategoryProduct.ftl"
            )}))})
        }
    )
    public interface EditCategoryProducts {}

    @Screen(name = "EditCategoryAttributes", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCategoryAttributes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategoryAttributes")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.SET, field = "title", defaultValue = "${uiLabelMap.ProductCategoryAttributes} ${productCategoryId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryAttribute", list = "categoryAttributes", conditions = {@ConditionExpr(fieldName = "productCategoryId", fromField = "productCategoryId")})
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CreateProductCategoryAttribute", location = "component://product/widget/catalog/CategoryForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductCategoryAttributeList}", includeForms = {
                    @IncludeForm(name = "EditProductCategoryAttributes", location = "component://product/widget/catalog/CategoryForms.xml"
                )})})
        }
    )
    public interface EditCategoryAttributes {}

    @Screen(name = "createProductInCategoryStart", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateProductCategoryStart")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCreateProductInCategory")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/createProductInCategoryStart.ftl"
            )})
        }
    )
    public interface createProductInCategoryStart {}

    @Screen(name = "CreateProductInCategoryCheckExisting", location = "component://product/widget/catalog/CategoryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateProductCategoryCheckExisting")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCreateProductInCategory")
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/CreateProductInCategoryCheckExisting.groovy")
    @DecoratorScreen(
        name = "CommonCategoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/createProductInCategoryCheckExisting.ftl"
            )})
        }
    )
    public interface CreateProductInCategoryCheckExisting {}

    @Screen(name = "EditCategoryStcLocFields", location = "component://product/widget/catalog/CategoryScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productCategory"})}))
    @Action(type = ActionType.SET, field = "productCategoryId", fromField = "productCategory.productCategoryId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/GetCategoryStcLocFields.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/category/EditCategoryStcLocFields.ftl")}))
    public interface EditCategoryStcLocFields {}

}
