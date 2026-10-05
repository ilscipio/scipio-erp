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
public class CatalogPromoForms {

    @Form(
        name = "ListProductPromos",
        location = "component://product/widget/catalog/PromoForms.xml",
        type = FormType.LIST,
        listName = "productPromos",
        paginateTarget = "FindProductPromo",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productPromoId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductPromo", description = "${productPromoId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoId")})),
            @FormField(name = "promoName", display = @DisplayField),
            @FormField(name = "promoText", encodeOutput = false, display = @DisplayField),
            @FormField(name = "requireCode", display = @DisplayField),
            @FormField(name = "createdDate", display = @DisplayField)
        }
    )
    public interface ListProductPromos {}

    @Form(
        name = "GoToProductPromoCode",
        location = "component://product/widget/catalog/PromoForms.xml",
        target = "EditProductPromoCode",
        headerRowStyle = "header-row",
        method = "get",
        fields = {
            @FormField(name = "productPromoCodeId", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_nav} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface GoToProductPromoCode {}

    @Form(
        name = "EditProductPromo",
        location = "component://product/widget/catalog/PromoForms.xml",
        target = "updateProductPromo",
        defaultMapName = "productPromo",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductPromo")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "productPromo==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "promoName", requiredField = true, text = @TextField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.ProductPromotion}", useWhen = "productPromo!=null", display = @DisplayField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.ProductPromotion}", tooltip = "${uiLabelMap.ProductCouldNotFindProductPromotion} [${productPromoId}]", useWhen = "productPromo==null&&productPromoId!=null", display = @DisplayField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.ProductPromotion}", useWhen = "productPromo==null&&productPromoId==null", ignored = @IgnoredField),
            @FormField(name = "promoText", title = "${uiLabelMap.ProductPromoText}", textarea = @TextareaField(cols = 70, rows = 5)),
            @FormField(name = "userEntered", title = "${uiLabelMap.ProductPromoUserEntered}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showToCustomer", title = "${uiLabelMap.ProductPromoShowToCustomer}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireCode", title = "${uiLabelMap.ProductPromotionReqCode}", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonN}"), @Option(key = "Y", description = "${uiLabelMap.CommonY}")})),
            @FormField(name = "overrideOrgPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productPromo==null", target = "createProductPromo")
        }
    )
    public interface EditProductPromo {}

    @Form(
        name = "EditProductPromoCode",
        location = "component://product/widget/catalog/PromoForms.xml",
        target = "updateProductPromoCode",
        defaultMapName = "productPromoCode",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductPromoCode")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "productPromoCode==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "productPromoId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPromo", description = "[${productPromoId}] ${promoName}", orderBy = {@EntityOrderBy(fieldName = "productPromoId")}))),
            @FormField(name = "productPromoCodeId", useWhen = "productPromoCode!=null", display = @DisplayField),
            @FormField(name = "productPromoCodeId", tooltip = "${uiLabelMap.ProductCouldNotFindProductPromoCode} [${productPromoCodeId}]", useWhen = "productPromoCode==null&&productPromoCodeId!=null", display = @DisplayField),
            @FormField(name = "productPromoCodeId", tooltip = "${uiLabelMap.ProductPromoCodeBlank}", useWhen = "productPromoCode==null&&productPromoCodeId==null", text = @TextField),
            @FormField(name = "userEntered", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireEmailOrParty", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonN}"), @Option(key = "Y", description = "${uiLabelMap.CommonY}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "lastUpdatedByText", title = "${uiLabelMap.ProductLastModifiedBy}:", useWhen = "productPromoCode!=null", display = @DisplayField(description = "[${productPromoCode.lastModifiedByUserLogin}] ${uiLabelMap.CommonOn} ${productPromoCode.lastModifiedDate}", alsoHidden = false)),
            @FormField(name = "createdByText", title = "${uiLabelMap.CommonCreatedBy}:", useWhen = "productPromoCode!=null", display = @DisplayField(description = "[${productPromoCode.createdByUserLogin}] ${uiLabelMap.CommonOn} ${productPromoCode.createdDate}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "productPromoCode==null", target = "createProductPromoCode")
        }
    )
    public interface EditProductPromoCode {}

    @Form(
        name = "ListProductPromoCodes",
        location = "component://product/widget/catalog/PromoForms.xml",
        type = FormType.LIST,
        listName = "productPromoCodes",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductPromoCode", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "lastModifiedDate", ignored = @IgnoredField),
            @FormField(name = "fromDate", ignored = @IgnoredField),
            @FormField(name = "createdDate", ignored = @IgnoredField),
            @FormField(name = "lastModifiedByUserLogin", ignored = @IgnoredField),
            @FormField(name = "productPromoId", hidden = @HiddenField),
            @FormField(name = "productPromoCodeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductPromoCode", description = "${productPromoCodeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoCodeId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPromoCode", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoCodeId"), @ParameterDef(paramName = "productPromoId")}))
        }
    )
    public interface ListProductPromoCodes {}

    @Form(
        name = "EditProductPromoContentImage",
        location = "component://product/widget/catalog/PromoForms.xml",
        type = FormType.UPLOAD,
        target = "addImageContentForProductPromo",
        defaultMapName = "productPromoContent",
        fields = {
            @FormField(name = "productPromoId", hidden = @HiddenField),
            @FormField(name = "contentId", useWhen = "productPromoContent != null", display = @DisplayField),
            @FormField(name = "productPromoContentTypeId", hidden = @HiddenField(value = "ORIGINAL_IMAGE_URL")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productPromoContent == null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productPromoContent != null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "uploadedFile", title = "${uiLabelMap.ProductFile}", requiredField = true, file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "productPromoContent == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "productPromoContent != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductPromoContentImage {}

    @Form(
        name = "ListProductPromoContent",
        location = "component://product/widget/catalog/PromoForms.xml",
        type = FormType.LIST,
        listName = "productPromoContents",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "editProductPromoContent", title = "${uiLabelMap.ProductContent}", widgetStyle = "${styles.link_nav_info_desc}", hyperlink = @HyperlinkField(target = "EditProductPromoContent", description = "${description} [${contentId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "productPromoContentTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "productPromoContentTypeId", title = "${uiLabelMap.ProductProductPromoContentType}", displayEntity = @DisplayEntityField(entityName = "ProductContentType", keyFieldName = "productContentTypeId", description = "${description}", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "editContentAction", title = "${uiLabelMap.ProductEditContent}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "/content/control/EditContent", urlMode = UrlMode.INTER_APP, description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "removeContentAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentFromProductPromo", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "productPromoContentTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductPromoContent {}

}
