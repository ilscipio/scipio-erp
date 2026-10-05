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
package com.ilscipio.scipio.content.widget;

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
public class WebsiteWebSiteForms {

    @Form(
        name = "EditWebSite",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "updateWebSite",
        defaultMapName = "webSite",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWebSite")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "webSite==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "webSiteId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "webSite!=null", display = @DisplayField),
            @FormField(name = "webSiteId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${webSiteId}]", useWhen = "webSite==null&&webSiteId!=null", requiredField = true, text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "webSiteId", useWhen = "webSite==null&&webSiteId==null", requiredField = true, text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "siteName", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "enableHttps", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "productStoreId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "visualThemeSetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "VisualThemeSet", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "webappPathPrefix", tooltip = "${uiLabelMap.ContentWebSiteWebappPathPrefixDesc}", text = @TextField),
            @FormField(name = "allowProductStoreChange", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isDefault", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isStoreDefault", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "webSite==null", target = "createWebSite")
        }
    )
    public interface EditWebSite {}

    @Form(
        name = "ListWebSites",
        location = "component://content/widget/website/WebSiteForms.xml",
        type = FormType.LIST,
        listName = "webSites",
        paginate = "true",
        paginateTarget = "FindWebSite",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "webSiteId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditWebSite", description = "${webSiteId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "webSiteId")})),
            @FormField(name = "siteName", title = "${uiLabelMap.CommonName}", sortField = true, display = @DisplayField),
            @FormField(name = "httpsHost", sortField = true, display = @DisplayField),
            @FormField(name = "httpsPort", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-webSiteId")})
    )
    public interface ListWebSites {}

    @Form(
        name = "FindWebSitePathAlias",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "WebSiteAliases",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "webSiteId", hidden = @HiddenField(value = "${webSiteId}")),
            @FormField(name = "pathAlias", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindWebSitePathAlias {}

    @Form(
        name = "ListWebSitePathAlias",
        location = "component://content/widget/website/WebSiteForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "WebSiteAliases",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "pathAlias", sortField = true, display = @DisplayField),
            @FormField(name = "pathAliasTo", sortField = true, display = @DisplayField),
            @FormField(name = "mapKey", sortField = true, display = @DisplayField),
            @FormField(name = "contentId", sortField = true, displayEntity = @DisplayEntityField(entityName = "Content", description = "${contentName}", subHyperlink = @SubHyperlink(target = "EditContent", description = " [${contentId}]", parameters = {@ParameterDef(paramName = "contentId")})))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "WebSitePathAlias"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "WebSiteAliasesSearchResults")
        }
    )
    public interface ListWebSitePathAlias {}

    @Form(
        name = "ListWebSiteContent",
        location = "component://content/widget/website/WebSiteForms.xml",
        type = FormType.LIST,
        target = "UpdateWebSiteContent",
        listName = "webSiteContent",
        paginate = "true",
        paginateTarget = "ListWebSiteContent",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWebSiteRole")
        },
        fields = {
            @FormField(name = "sequenceNum", hidden = @HiddenField),
            @FormField(name = "roleTypeId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "webSiteId", display = @DisplayField),
            @FormField(name = "contentId", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "EditContent", description = "[${contentId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "webSiteContentTypeId", displayEntity = @DisplayEntityField(entityName = "WebSiteContentType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveWebSiteContent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "webSiteId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "webSiteContentTypeId"), @ParameterDef(paramName = "fromDate")}))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "webSiteId"), @SortField(name = "contentId"), @SortField(name = "webSiteContentTypeId"), @SortField(name = "fromDate"), @SortField(name = "thruDate"), @SortField(name = "submitAction"), @SortField(name = "deleteAction")})
    )
    public interface ListWebSiteContent {}

    @Form(
        name = "CreateWebSiteContent",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "CreateWebSiteContent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWebSiteContent")
        },
        fields = {
            @FormField(name = "webSiteId", mapName = "webSite", display = @DisplayField),
            @FormField(name = "contentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "webSiteContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WebSiteContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateWebSiteContent {}

    @Form(
        name = "CreateWebSiteRole",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "createWebSiteRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWebSiteRole")
        },
        fields = {
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "webSiteId", mapName = "webSite", hidden = @HiddenField),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateWebSiteRole {}

    @Form(
        name = "UpdateWebSiteRole",
        location = "component://content/widget/website/WebSiteForms.xml",
        type = FormType.LIST,
        target = "updateWebSiteRole",
        listName = "webSiteRoleDatas",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWebSiteRole", mapName = "webSiteRole")
        },
        fields = {
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${person.personalTitle} ${person.firstName} ${person.middleName} ${person.lastName} ${person.suffix} ${partyGroup.groupName} [${webSiteRole.partyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "webSiteRole.partyId")})),
            @FormField(name = "roleTypeId", display = @DisplayField(description = "${roleType.description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeWebSiteRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "webSiteId", fromField = "webSiteRole.webSiteId"), @ParameterDef(paramName = "partyId", fromField = "webSiteRole.partyId"), @ParameterDef(paramName = "roleTypeId", fromField = "webSiteRole.roleTypeId"), @ParameterDef(paramName = "fromDate", fromField = "webSiteRole.fromDate")}))
        }
    )
    public interface UpdateWebSiteRole {}

    @Form(
        name = "AutoCreateWebsiteContent",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "autoCreateWebSiteContent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "webSiteContentTypeId", check = @CheckField(entityOptions = @EntityOptions(entityName = "WebSiteContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AutoCreateWebsiteContent {}

    @Form(
        name = "CreateWebsiteSEO",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "generateMissingSeoUrlForWebsite",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "serviceMode", title = "${uiLabelMap.CommonMode}", dropDown = @DropDownField(options = {@Option(key = "sync", description = "Sync"), @Option(key = "async-onetime", description = "Async (${uiLabelMap.CommonOneTimeExecNotPersistedResultsInLog})")})),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.Catalog}", dropDown = @DropDownField(options = {@Option(key = "all", description = "All")}, entityOptions = @EntityOptions(entityName = "ProductStoreCatalog", description = "${prodCatalogId}", constraints = {@EntityConstraint(name = "productStoreId", value = "${webSite.productStoreId}")}))),
            @FormField(name = "typeGenerate", title = "${uiLabelMap.ContentGenerateType}", check = @CheckField(allChecked = true, options = {@Option(key = "category", description = "Category"), @Option(key = "product", description = "Product")})),
            @FormField(name = "replaceExisting", title = "${uiLabelMap.ContentReplaceExisting}", check = @CheckField(allChecked = true)),
            @FormField(name = "removeOldLocales", title = "${uiLabelMap.ContentRemoveOldLocales}", check = @CheckField(allChecked = true)),
            @FormField(name = "includeVariant", title = "${uiLabelMap.ContentIncludeVariantProducts}", check = @CheckField(allChecked = true)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateWebsiteSEO {}

    @Form(
        name = "CreateWebSiteContactList",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "createWebSiteContactList",
        defaultMapName = "webSite",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "webSiteId", display = @DisplayField),
            @FormField(name = "siteName", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField(value = "${fromDate}")),
            @FormField(name = "contactListId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactList", description = "${contactListName} [${contactListId}]", keyFieldName = "contactListId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "fromDate", value = "${groovy: import org.ofbiz.base.util.UtilDateTime; return UtilDateTime.nowTimestamp();}", type = "Timestamp")})
    )
    public interface CreateWebSiteContactList {}

    @Form(
        name = "ViewWebSiteContactList",
        location = "component://content/widget/website/WebSiteForms.xml",
        type = FormType.LIST,
        target = "updateWebSiteContactList",
        listName = "webSiteContactLists",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", hidden = @HiddenField),
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "contactListName", displayEntity = @DisplayEntityField(entityName = "ContactList", keyFieldName = "contactListId", description = "${contactListName}", subHyperlink = @SubHyperlink(target = "/marketing/control/EditContactList", description = "[${contactListId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contactListId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWebSiteContactList", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "webSiteId"), @ParameterDef(paramName = "contactListId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ContactList", valueField = "contactList")}),
        rowActions = @RowActions(set = {@SetAction(field = "contactListName", fromField = "contactList.contactListName"), @SetAction(field = "description", fromField = "contactList.description")}, entityOne = {@EntityOneAction(entityName = "ContactList", valueField = "contactList")})
    )
    public interface ViewWebSiteContactList {}

    @Form(
        name = "CreateWebsiteSitemaps",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "generateSitemapFilesForWebsite",
        fields = {
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "serviceMode", title = "${uiLabelMap.CommonMode}", dropDown = @DropDownField(options = {@Option(key = "sync", description = "Sync"), @Option(key = "async-onetime", description = "Async (${uiLabelMap.CommonOneTimeExecNotPersistedResultsInLog})")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateWebsiteSitemaps {}

    @Form(
        name = "RemoveWebsiteSEO",
        location = "component://content/widget/website/WebSiteForms.xml",
        target = "removeSeoUrlForWebsite",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "serviceMode", title = "${uiLabelMap.CommonMode}", dropDown = @DropDownField(options = {@Option(key = "sync", description = "Sync"), @Option(key = "async-onetime", description = "Async (${uiLabelMap.CommonOneTimeExecNotPersistedResultsInLog})")})),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.Catalog}", dropDown = @DropDownField(options = {@Option(key = "all", description = "All")}, entityOptions = @EntityOptions(entityName = "ProductStoreCatalog", description = "${prodCatalogId}", constraints = {@EntityConstraint(name = "productStoreId", value = "${webSite.productStoreId}")}))),
            @FormField(name = "targetTypes", title = "${uiLabelMap.CommonType}", check = @CheckField(allChecked = true, options = {@Option(key = "category", description = "Category"), @Option(key = "product", description = "Product")})),
            @FormField(name = "includeVariant", title = "${uiLabelMap.ContentIncludeVariantProducts}", check = @CheckField(allChecked = true)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface RemoveWebsiteSEO {}

}
