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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogConfigForms {

    @Form(
        name = "FindProductConfigItems",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "FindProductConfigItems",
        defaultMapName = "productconfigitems",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "configItemName", title = "${uiLabelMap.CommonName}", position = 2, textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "configItemTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(options = {@Option(key = "SINGLE", description = "${uiLabelMap.ProductSingleChoice}"), @Option(key = "MULTIPLE", description = "${uiLabelMap.ProductMultiChoice}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindProductConfigItems {}

    @Form(
        name = "ListProductConfigItems",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "true",
        paginateTarget = "FindProductConfigItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductConfigItem", description = "${configItemId}", parameters = {@ParameterDef(paramName = "configItemId")})),
            @FormField(name = "configItemName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "typeDescription", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "${typeDescription}")),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "performFindResult", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductConfigItem"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "typeDescription", value = "${groovy: return \"SINGLE\".equals(configItemTypeId) ? uiLabelMap.get(\"ProductSingleChoice\") : uiLabelMap.get(\"ProductMultiChoice\")}")})
    )
    public interface ListProductConfigItems {}

    @Form(
        name = "EditProductConfigItem",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "updateProductConfigItem",
        defaultMapName = "configItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "longDescription", title = "${uiLabelMap.ProductLongDescription}", ignored = @IgnoredField),
            @FormField(name = "imageUrl", ignored = @IgnoredField),
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonId}", tooltip = "${uiLabelMap.ProductNotModificationRecreatingProductConfigItems}", useWhen = "configItem!=null", display = @DisplayField),
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonId}", tooltip = "${uiLabelMap.ProductCouldNotFindProductConfigItemWithId} [${configItemId}]", useWhen = "configItem==null&&configItemId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonId}", useWhen = "configItem==null&&configItemId==null", ignored = @IgnoredField),
            @FormField(name = "configItemTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(options = {@Option(key = "SINGLE", description = "${uiLabelMap.ProductSingleChoice}"), @Option(key = "MULTIPLE", description = "${uiLabelMap.ProductMultiChoice}")})),
            @FormField(name = "configItemName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", position = 2, textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "configItem==null", target = "createProductConfigItem")
        }
    )
    public interface EditProductConfigItem {}

    @Form(
        name = "EditConfigOption",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        target = "updateProductConfigOption",
        listName = "configOptionList",
        listEntryName = "configOption",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductConfigOption", mapName = "configOption")
        },
        fields = {
            @FormField(name = "configOptionId", useWhen = "configOption!=null", hidden = @HiddenField),
            @FormField(name = "configItemId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditConfigOption {}

    @Form(
        name = "CreateConfigOption",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "createProductConfigOption",
        defaultMapName = "configOption",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductConfigOption")
        },
        fields = {
            @FormField(name = "configOptionId", useWhen = "configOption!=null", hidden = @HiddenField),
            @FormField(name = "configItemId", hidden = @HiddenField(value = "${configItemId}")),
            @FormField(name = "configOptionName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "configOption!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "configOption!=null", target = "updateProductConfigOption")
        }
    )
    public interface CreateConfigOption {}

    @Form(
        name = "CreateProductConfigProduct",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "createProductConfigProduct",
        defaultMapName = "productConfigProduct",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductConfigProduct")
        },
        fields = {
            @FormField(name = "configItemId", hidden = @HiddenField(value = "${configItemId}")),
            @FormField(name = "configOptionId", hidden = @HiddenField(value = "${configOptionId}")),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", useWhen = "productConfigProduct!=null", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", useWhen = "productConfigProduct==null", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "productConfigProduct!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productConfigProduct!=null", target = "updateProductConfigProduct")
        }
    )
    public interface CreateProductConfigProduct {}

    @Form(
        name = "AddProductConfigItemContentAssoc",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "addContentToProductConfigItem",
        title = "Add ProdConfItemContent (select Content Id, enter From Date):",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProdConfItemContent")
        },
        fields = {
            @FormField(name = "configItemId", mapName = "productConfigItem", title = "${uiLabelMap.ProductConfigItemId}", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}"),
            @FormField(name = "confItemContentTypeId", title = "${uiLabelMap.ProductProductConfigItemContentTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdConfItemContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductConfigItemContentAssoc {}

    @Form(
        name = "PrepareAddProductConfigItemContentAssoc",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "prepareAddContentToProductConfigItem",
        title = "Add ProdConfItemContent (select Content Id, enter From Date):",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProdConfItemContent")
        },
        fields = {
            @FormField(name = "contentId", ignored = @IgnoredField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", ignored = @IgnoredField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, ignored = @IgnoredField),
            @FormField(name = "configItemId", mapName = "productConfigItem", hidden = @HiddenField),
            @FormField(name = "confItemContentTypeId", title = "${uiLabelMap.ProductProductConfigItemContentTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdConfItemContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductPrepareCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface PrepareAddProductConfigItemContentAssoc {}

    @Form(
        name = "UpdateProductConfigItemContentAssoc",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        target = "updateContentToProductConfigItem",
        listName = "productContentDatas",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductConfigItemContent", mapName = "productContent")
        },
        fields = {
            @FormField(name = "configItemId", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContent_Id}", widgetStyle = "${styles.link_nav_info_desc}", hyperlink = @HyperlinkField(target = "EditProductConfigItemContentContent", description = "${content.description} [${productContent.contentId}]", parameters = {@ParameterDef(paramName = "configItemId", fromField = "productContent.configItemId"), @ParameterDef(paramName = "contentId", fromField = "productContent.contentId")})),
            @FormField(name = "confItemContentTypeId", title = "${uiLabelMap.ProductProductContentTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdConfItemContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentFromProductConfigItem", description = "[${uiLabelMap.CommonDelete}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "configItemId", fromField = "productContent.configItemId"), @ParameterDef(paramName = "contentId", fromField = "productContent.contentId"), @ParameterDef(paramName = "confItemContentTypeId", fromField = "productContent.confItemContentTypeId"), @ParameterDef(paramName = "fromDate", fromField = "productContent.fromDate")}))
        }
    )
    public interface UpdateProductConfigItemContentAssoc {}

    @Form(
        name = "EditProductConfigItemContentSimpleText",
        location = "component://product/widget/catalog/ConfigForms.xml",
        target = "updateSimpleTextContentForProductConfigItem",
        title = "Update Simple Text Content for Product",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProdConfItemContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "localeString", mapName = "content", title = "${uiLabelMap.ProductLocaleString}", text = @TextField(size = 40)),
            @FormField(name = "contentId", useWhen = "contentId == null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "productContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "text", mapName = "textData", title = "${uiLabelMap.ProductText}", textarea = @TextareaField(rows = 7)),
            @FormField(name = "textDataResourceId", mapName = "textData", title = "${uiLabelMap.ProductTextDataResourceId}", hidden = @HiddenField),
            @FormField(name = "configItemId", hidden = @HiddenField),
            @FormField(name = "confItemContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createSimpleTextContentForProductConfigItem")
        }
    )
    public interface EditProductConfigItemContentSimpleText {}

    @Form(
        name = "ListProductConfigItem",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        listName = "productConfigs",
        paginate = "true",
        paginateTarget = "FindProductConfigItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductConfigAndProduct", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductConfigs", description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${productName}")),
            @FormField(name = "piecesIncluded", title = "${uiLabelMap.ProductPiecesIncluded}", display = @DisplayField(description = "${piecesIncluded}"))
        }
    )
    public interface ListProductConfigItem {}

    @Form(
        name = "ProductConfigOptionList",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        listName = "configOptionList",
        paginate = "true",
        paginateTarget = "FindProductConfigItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductConfigOption", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "configItemId", title = "${uiLabelMap.ProductConfigItem}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditProductConfigOptions", description = "${configOptionId} - ${configOptionName}", parameters = {@ParameterDef(paramName = "configItemId"), @ParameterDef(paramName = "configOptionId")})),
            @FormField(name = "configOptionId", hidden = @HiddenField),
            @FormField(name = "configOptionName", hidden = @HiddenField),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductConfigOption", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "configItemId"), @ParameterDef(paramName = "configOptionId")}))
        }
    )
    public interface ProductConfigOptionList {}

    @Form(
        name = "ProductConfigList",
        location = "component://product/widget/catalog/ConfigForms.xml",
        type = FormType.LIST,
        listName = "configProducts",
        paginate = "true",
        paginateTarget = "FindProductConfigItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductConfigProduct", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "configItemId", hidden = @HiddenField),
            @FormField(name = "configOptionId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditProduct", description = "${product.productId} - ${product.productName}", parameters = {@ParameterDef(paramName = "productId", fromField = "product.productId")})),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductConfigProduct", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "configItemId"), @ParameterDef(paramName = "configOptionId"), @ParameterDef(paramName = "productId", fromField = "product.productId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product")})
    )
    public interface ProductConfigList {}

}
