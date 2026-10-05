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
public class CatalogReviewForms {

    @Form(
        name = "FindReviews",
        location = "component://product/widget/catalog/ReviewForms.xml",
        target = "FindReviews",
        defaultMapName = "productReview",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductId}", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "PRODUCT_REVIEW_STTS")}))),
            @FormField(name = "productReview", title = "${uiLabelMap.ProductReviews}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindReviews {}

    @Form(
        name = "ListReviews",
        location = "component://product/widget/catalog/ReviewForms.xml",
        type = FormType.LIST,
        target = "updateProductReview",
        listName = "listIt",
        paginateTarget = "FindReviews",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewProduct", description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "userLoginId", title = "${uiLabelMap.ProductReviewBy}", display = @DisplayField),
            @FormField(name = "productRating", useWhen = "${statusId != 'PRR_DELETED'}", text = @TextField),
            @FormField(name = "productRating", useWhen = "${statusId == 'PRR_DELETED'}", text = @TextField(disabled = true)),
            @FormField(name = "productReview", useWhen = "${statusId != 'PRR_DELETED'}", textarea = @TextareaField),
            @FormField(name = "productReview", useWhen = "${statusId == 'PRR_DELETED'}", textarea = @TextareaField(readonly = true)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "${statusId != 'PRR_DELETED'}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "approveAction", useWhen = "${statusId == 'PRR_PENDING'}", widgetStyle = "${styles.link_run_sys} ${styles.action_updatestatus}", hyperlink = @HyperlinkField(target = "updateProductReviewStatus", description = "${uiLabelMap.FormFieldTitle_approve}", alsoHidden = false, parameters = {@ParameterDef(paramName = "statusId", value = "PRR_APPROVED"), @ParameterDef(paramName = "productReviewId", fromField = "productReviewId")})),
            @FormField(name = "rejectAction", useWhen = "${statusId != 'PRR_DELETED'}", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "updateProductReviewStatus", description = "${uiLabelMap.FormFieldTitle_rejectButton}", alsoHidden = false, confirmationMessage = "Do you want to reject this review?", parameters = {@ParameterDef(paramName = "statusId", value = "PRR_DELETED"), @ParameterDef(paramName = "productReviewId", fromField = "productReviewId")})),
            @FormField(name = "productReviewId", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "ProductReview")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", value = "postedDateTime"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "reviewBy", value = "${userLoginAndPartyDetails[0].firstName} ${userLoginAndPartyDetails[0].middleName} ${userLoginAndPartyDetails[0].lastName}")})
    )
    public interface ListReviews {}

}
