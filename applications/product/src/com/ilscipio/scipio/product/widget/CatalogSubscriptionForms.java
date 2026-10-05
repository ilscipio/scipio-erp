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
public class CatalogSubscriptionForms {

    @Form(
        name = "FindSubscription",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "FindSubscription",
        defaultMapName = "subscription",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Subscription", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "subscriptionResourceId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SubscriptionResource", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "subscriptionTypeId", title = "${uiLabelMap.ProductSubscriptionType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SubscriptionType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "originatedFromPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "originatedFromRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productCategoryId", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "automaticExtend", title = "Automatic Extend", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "roleTypeId", ignored = @IgnoredField),
            @FormField(name = "canclAutmExtTimeUomId", ignored = @IgnoredField),
            @FormField(name = "canclAutmExtTime", ignored = @IgnoredField),
            @FormField(name = "originatedFromPartyId", ignored = @IgnoredField),
            @FormField(name = "originatedFromRoleTypeId", ignored = @IgnoredField),
            @FormField(name = "contactMechId", ignored = @IgnoredField),
            @FormField(name = "communicationEventId", ignored = @IgnoredField),
            @FormField(name = "productCategoryId", ignored = @IgnoredField),
            @FormField(name = "inventoryItemId", ignored = @IgnoredField),
            @FormField(name = "availableTime", ignored = @IgnoredField),
            @FormField(name = "availableTimeUomId", ignored = @IgnoredField),
            @FormField(name = "partyNeedId", ignored = @IgnoredField),
            @FormField(name = "needTypeId", ignored = @IgnoredField),
            @FormField(name = "useCountLimit", ignored = @IgnoredField),
            @FormField(name = "maxLifeTime", ignored = @IgnoredField),
            @FormField(name = "maxLifeTimeUomId", ignored = @IgnoredField),
            @FormField(name = "maxUseTimeUomId", ignored = @IgnoredField),
            @FormField(name = "useTime", ignored = @IgnoredField),
            @FormField(name = "useTimeUomId", ignored = @IgnoredField),
            @FormField(name = "purchaseFromDate", ignored = @IgnoredField),
            @FormField(name = "purchaseThruDate", ignored = @IgnoredField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSubscription {}

    @Form(
        name = "ListFindSubscription",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "true",
        paginateTarget = "FindSubscription",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Subscription", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "subscriptionResourceId", displayEntity = @DisplayEntityField(entityName = "SubscriptionResource", description = "${description}", subHyperlink = @SubHyperlink(target = "EditSubscriptionResource", description = "${subscriptionResourceId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "subscriptionResourceId")}))),
            @FormField(name = "subscriptionTypeId", title = "${uiLabelMap.ProductSubscriptionType}", displayEntity = @DisplayEntityField(entityName = "SubscriptionType", description = "${description}")),
            @FormField(name = "originatedFromPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${originatedFromPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "originatedFromPartyId")}))),
            @FormField(name = "originatedFromRoleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName} ${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", description = "${productName}", subHyperlink = @SubHyperlink(target = "/catalog/control/ViewProduct", description = "${productId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productId")}))),
            @FormField(name = "productCategoryId", displayEntity = @DisplayEntityField(entityName = "ProductCategory", description = "${categoryName}", subHyperlink = @SubHyperlink(target = "/catalog/control/EditCategory", description = "${productCategoryId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productCategoryId")}))),
            @FormField(name = "roleTypeId", ignored = @IgnoredField),
            @FormField(name = "canclAutmExtTimeUomId", ignored = @IgnoredField),
            @FormField(name = "canclAutmExtTime", ignored = @IgnoredField),
            @FormField(name = "originatedFromPartyId", ignored = @IgnoredField),
            @FormField(name = "originatedFromRoleTypeId", ignored = @IgnoredField),
            @FormField(name = "contactMechId", ignored = @IgnoredField),
            @FormField(name = "communicationEventId", ignored = @IgnoredField),
            @FormField(name = "productCategoryId", ignored = @IgnoredField),
            @FormField(name = "inventoryItemId", ignored = @IgnoredField),
            @FormField(name = "availableTime", ignored = @IgnoredField),
            @FormField(name = "availableTimeUomId", ignored = @IgnoredField),
            @FormField(name = "partyNeedId", ignored = @IgnoredField),
            @FormField(name = "needTypeId", ignored = @IgnoredField),
            @FormField(name = "useCountLimit", ignored = @IgnoredField),
            @FormField(name = "maxLifeTime", ignored = @IgnoredField),
            @FormField(name = "maxLifeTimeUomId", ignored = @IgnoredField),
            @FormField(name = "maxUseTimeUomId", ignored = @IgnoredField),
            @FormField(name = "useTime", ignored = @IgnoredField),
            @FormField(name = "useTimeUomId", ignored = @IgnoredField),
            @FormField(name = "purchaseFromDate", ignored = @IgnoredField),
            @FormField(name = "purchaseThruDate", ignored = @IgnoredField),
            @FormField(name = "subscriptionId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSubscription", description = "${subscriptionId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "subscriptionId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "performFindResult", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Subscription"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFindSubscription {}

    @Form(
        name = "EditSubscription",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "updateSubscription",
        defaultMapName = "subscription",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSubscription")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "subscription!=null", hidden = @HiddenField(value = "true")),
            @FormField(name = "subscriptionId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "subscription!=null", display = @DisplayField),
            @FormField(name = "subscriptionId", useWhen = "subscription==null&&subscriptionId==null", ignored = @IgnoredField),
            @FormField(name = "subscriptionId", useWhen = "subscription==null&&subscriptionId!=null", display = @DisplayField(description = "${uiLabelMap.CommonCannotBeFound}: [${subscriptionId}]", alsoHidden = false)),
            @FormField(name = "subscriptionResourceId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SubscriptionResource", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "subscriptionTypeId", title = "${uiLabelMap.ProductSubscriptionType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SubscriptionType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "originatedFromPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "originatedFromRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyNeedId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "needTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "NeedType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productCategoryId", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "useTimeUomId", title = "${uiLabelMap.ProductUseTimeUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "automaticExtend", title = "Automatic Extend", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "canclAutmExtTime", title = "Cancel time"),
            @FormField(name = "canclAutmExtTimeUomId", title = "Cancel UOM time", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "gracePeriodOnExpiryUomId", title = "${uiLabelMap.ProductGracePeriodUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "subscription==null", target = "createSubscription")
        }
    )
    public interface EditSubscription {}

    @Form(
        name = "ListSubscriptionResources",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        type = FormType.LIST,
        listName = "examples",
        paginateTarget = "FindSubscriptionResource",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SubscriptionResource", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "parentResourceId", ignored = @IgnoredField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "contentId", displayEntity = @DisplayEntityField(entityName = "Content", description = "${contentName}", subHyperlink = @SubHyperlink(target = "/content/control/EditContent", description = "${contentId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "webSiteId", displayEntity = @DisplayEntityField(entityName = "WebSite", description = "${siteName}", subHyperlink = @SubHyperlink(target = "/content/control/EditWebSite", description = "${webSiteId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "webSiteId")}))),
            @FormField(name = "subscriptionResourceId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSubscriptionResource", description = "${subscriptionResourceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "subscriptionResourceId")}))
        }
    )
    public interface ListSubscriptionResources {}

    @Form(
        name = "EditSubscriptionResource",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "updateSubscriptionResource",
        defaultMapName = "subscriptionResource",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSubscriptionResource")
        },
        fields = {
            @FormField(name = "subscriptionResourceId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "subscriptionResource!=null", display = @DisplayField),
            @FormField(name = "subscriptionResourceId", useWhen = "subscriptionResource==null&&subscriptionResourceId==null", ignored = @IgnoredField),
            @FormField(name = "subscriptionResourceId", useWhen = "subscriptionResource==null&&subscriptionResourceId!=null", display = @DisplayField(description = "${uiLabelMap.CommonCannotBeFound}: [${subscriptionResourceId}]", alsoHidden = false)),
            @FormField(name = "parentResourceId", ignored = @IgnoredField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "contentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "webSiteId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WebSite", description = "${siteName} [${webSiteId}]", orderBy = {@EntityOrderBy(fieldName = "siteName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "subscriptionResource==null", target = "createSubscriptionResource")
        }
    )
    public interface EditSubscriptionResource {}

    @Form(
        name = "ListSubscriptionResourceProducts",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        type = FormType.LIST,
        target = "updateProductSubscriptionResourceSr",
        listName = "productSubscriptionResource",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductSubscriptionResource")
        },
        fields = {
            @FormField(name = "subscriptionResourceId", hidden = @HiddenField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "useTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "automaticExtend", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "canclAutmExtTimeUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductSubscriptionResourceSr", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "subscriptionResourceId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListSubscriptionResourceProducts {}

    @Form(
        name = "AddSubscriptionResourceProduct",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "createProductSubscriptionResourceSr",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductSubscriptionResource")
        },
        fields = {
            @FormField(name = "subscriptionResourceId", hidden = @HiddenField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "useTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "automaticExtend", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "canclAutmExtTimeUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSubscriptionResourceProduct {}

    @Form(
        name = "EditSubscriptionAttributes",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        type = FormType.LIST,
        target = "UpdateSubscriptionAttribute",
        listName = "subscriptionAttributes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSubscriptionAttribute")
        },
        fields = {
            @FormField(name = "subscriptionId", hidden = @HiddenField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditSubscriptionAttributes {}

    @Form(
        name = "AddSubscriptionAttribute",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "UpdateSubscriptionAttribute",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSubscriptionAttribute")
        },
        fields = {
            @FormField(name = "subscriptionId", hidden = @HiddenField),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSubscriptionAttribute {}

    @Form(
        name = "listSubscriptionCommEvent",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        type = FormType.LIST,
        target = "ListSubscriptionCommEvent",
        listName = "subscriptionCommEvent",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "subscriptionId", hidden = @HiddenField),
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/EditCommunicationEvent", urlMode = UrlMode.INTER_APP, description = "${communicationEventId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "communicationEventId")})),
            @FormField(name = "partyIdFrom", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "communicationEventTypeId", displayEntity = @DisplayEntityField(entityName = "CommunicationEventType", keyFieldName = "communicationEventTypeId", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeSubscriptionCommEvent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "subscriptionId"), @ParameterDef(paramName = "communicationEventId")}))
        }
    )
    public interface listSubscriptionCommEvent {}

    @Form(
        name = "createSubscriptionCommEvent",
        location = "component://product/widget/catalog/SubscriptionForms.xml",
        target = "createSubscriptionCommEvent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSubscriptionCommEvent")
        },
        fields = {
            @FormField(name = "subscriptionId", hidden = @HiddenField(value = "${parameters.subscriptionId}")),
            @FormField(name = "communicationEventId", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface createSubscriptionCommEvent {}

}
