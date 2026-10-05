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
package com.ilscipio.scipio.cms.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#main",
        controller = "cms"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "pagenotfound",
        type = "screen",
        page = "component://cms/widget/CommonScreens.xml#404",
        controller = "cms"
    )
    public static final String VIEW_PAGENOTFOUND = "pagenotfound";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "pages",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#pages",
        controller = "cms"
    )
    public static final String VIEW_PAGES = "pages";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editPage",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editPage",
        controller = "cms"
    )
    public static final String VIEW_EDITPAGE = "editPage";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "pageVersionList",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#pageVersionList",
        controller = "cms"
    )
    public static final String VIEW_PAGEVERSIONLIST = "pageVersionList";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "redirects",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#redirects",
        controller = "cms"
    )
    public static final String VIEW_REDIRECTS = "redirects";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "robots",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#robots",
        controller = "cms"
    )
    public static final String VIEW_ROBOTS = "robots";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "templates",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#templates",
        controller = "cms"
    )
    public static final String VIEW_TEMPLATES = "templates";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editTemplate",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editTemplate",
        controller = "cms"
    )
    public static final String VIEW_EDITTEMPLATE = "editTemplate";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "assets",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#assets",
        controller = "cms"
    )
    public static final String VIEW_ASSETS = "assets";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editAsset",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editAsset",
        controller = "cms"
    )
    public static final String VIEW_EDITASSET = "editAsset";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "contentAssets",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#contentAssets",
        controller = "cms"
    )
    public static final String VIEW_CONTENTASSETS = "contentAssets";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editContentAsset",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editContentAsset",
        controller = "cms"
    )
    public static final String VIEW_EDITCONTENTASSET = "editContentAsset";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "customImageSizePresets",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#customImageSizePresets",
        controller = "cms"
    )
    public static final String VIEW_CUSTOMIMAGESIZEPRESETS = "customImageSizePresets";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "scripts",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#scripts",
        controller = "cms"
    )
    public static final String VIEW_SCRIPTS = "scripts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editScript",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editScript",
        controller = "cms"
    )
    public static final String VIEW_EDITSCRIPT = "editScript";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "media",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#media",
        controller = "cms"
    )
    public static final String VIEW_MEDIA = "media";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editMedia",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#editMedia",
        controller = "cms"
    )
    public static final String VIEW_EDITMEDIA = "editMedia";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "menus",
        type = "screen",
        page = "component://cms/widget/CMSScreens.xml#menus",
        controller = "cms"
    )
    public static final String VIEW_MENUS = "menus";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "libMimeTypeExport",
        type = "screenxml",
        page = "component://cms/widget/CMSScreens.xml#libMimeTypeExport",
        controller = "cms"
    )
    public static final String VIEW_LIBMIMETYPEEXPORT = "libMimeTypeExport";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CmsDataImport",
            type = "screen",
            page = "component://cms/widget/CMSScreens.xml#CmsDataImport",
            controller = "cms"
        )
        public static final String VIEW_CMSDATAIMPORT = "CmsDataImport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CmsDataExport",
            type = "screen",
            page = "component://cms/widget/CMSScreens.xml#CmsDataExport",
            controller = "cms"
        )
        public static final String VIEW_CMSDATAEXPORT = "CmsDataExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CmsDataExportRaw",
            page = "/importexport/CmsDataExportRaw.jsp",
            controller = "cms"
        )
        public static final String VIEW_CMSDATAEXPORTRAW = "CmsDataExportRaw";

        @Request(
            uri = "main",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "pagenotfound",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "pagenotfound")
        public interface Pagenotfound {}

        @Request(
            uri = "pages",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "pages")
        public interface Pages {}

        @Request(
            uri = "editPage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPage")
        public interface EditPage {}

        @Request(
            uri = "pageVersionList",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "pageVersionList", allowViewSave = "false")
        public interface PageVersionList {}

        @Request(
            uri = "createPage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPage")
        @Response(name = "error", type = "view", value = "editPage")
        @Event(type = "service", invoke = "cmsCreatePage")
        public static String createPage(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "copyPage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editPage")
        @Response(name = "error", type = "view", value = "editPage")
        @Event(type = "service", invoke = "cmsCopyPage")
        public static String copyPage(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addScriptToPage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPage")
        @Response(name = "error", type = "view", value = "editPage")
        @Event(type = "service", invoke = "cmsUpdatePageScript")
        public static String addScriptToPage(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updatePageScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPage")
        @Response(name = "error", type = "view", value = "editPage")
        @Event(type = "service", invoke = "cmsUpdatePageScript")
        public static String updatePageScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteScriptAndPageAssoc",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPage")
        @Response(name = "error", type = "view", value = "editPage")
        @Event(type = "service", invoke = "cmsDeleteScriptAndPageAssoc")
        public static String deleteScriptAndPageAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getPages",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetPages")
        public static String getPages(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getCmsWebSites",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetCmsWebSites")
        public static String getCmsWebSites(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updatePageInfo",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsUpdatePageInfo")
        public static String updatePageInfo(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addPageVersion",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsAddPageVersion")
        public static String addPageVersion(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "activatePageVersion",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearCaches")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsActivatePageVersion")
        public static String activatePageVersion(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "unpublishPage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearCaches")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsUnpublishPage")
        public static String unpublishPage(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "clearCaches",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public interface ClearCaches {}

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @Request(
            uri = "addRemovePageViewMappings",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsAddRemovePageViewMappings")
        public static String addRemovePageViewMappings(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deletePage",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsDeletePage")
        public static String deletePage(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "robots",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "robots")
        public interface Robots {}

        @Request(
            uri = "updateRobots",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "robots")
        @Response(name = "error", type = "view", value = "robots")
        @Event(type = "service", invoke = "updateWebSite")
        public static String updateRobots(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "redirects",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "redirects")
        public interface Redirects {}

        @Request(
            uri = "getRedirects",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetRedirects")
        public static String getRedirects(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateRedirects",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "updateWebSite")
        public static String updateRedirects(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "templates",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "templates")
        public interface Templates {}

        @Request(
            uri = "createTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCreatePageTemplate")
        public static String createTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "copyTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCopyPageTemplate")
        public static String copyTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "editTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        public interface EditTemplate {}

        @Request(
            uri = "addAttributeToTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCreateUpdateAttribute")
        public static String addAttributeToTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateTemplateAttribute",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCreateUpdateAttribute")
        public static String updateTemplateAttribute(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteAttributeFromTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsDeleteAttribute")
        public static String deleteAttributeFromTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addScriptToTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsUpdatePageTemplateScript")
        public static String addScriptToTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateTemplateScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsUpdatePageTemplateScript")
        public static String updateTemplateScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteScriptAndPageTemplateAssoc",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsDeleteScriptAndPageTemplateAssoc")
        public static String deleteScriptAndPageTemplateAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deletePageTemplateScriptAssoc",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsDeletePageTemplateScriptAssoc")
        public static String deletePageTemplateScriptAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addAssetToTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCreateUpdateAssetAssoc")
        public static String addAssetToTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateTemplateAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsCreateUpdateAssetAssoc")
        public static String updateTemplateAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @Request(
            uri = "deleteAssetFromTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemplate")
        @Response(name = "error", type = "view", value = "editTemplate")
        @Event(type = "service", invoke = "cmsDeleteAssetAssoc")
        public static String deleteAssetFromTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateTemplateInfo",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsUpdatePageTemplateInfo")
        public static String updateTemplateInfo(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addTemplateVersion",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsAddPageTemplateVersion")
        public static String addTemplateVersion(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "activateTemplateVersion",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsActivatePageTemplateVersion")
        public static String activateTemplateVersion(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteTemplate",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsDeletePageTemplate")
        public static String deleteTemplate(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "assets",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "assets")
        public interface Assets {}

        @Request(
            uri = "editAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editAsset")
        public interface EditAsset {}

        @Request(
            uri = "contentAssets",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "contentAssets")
        public interface ContentAssets {}

        @Request(
            uri = "editContentAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editContentAsset")
        public interface EditContentAsset {}

        @Request(
            uri = "editAssetResponse",
            controller = "cms",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "editAsset")
        @Response(name = "content", type = "view", value = "editContentAsset")
        @Response(name = "error", type = "view", value = "editAsset")
        public interface EditAssetResponse {}

        @Request(
            uri = "editAssetRedirectResponse",
            controller = "cms",
            directRequest = "false"
        )
        @Response(name = "success", type = "request-redirect", value = "editAsset")
        @Response(name = "content", type = "request-redirect", value = "editContentAsset")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        public interface EditAssetRedirectResponse {}

        @Request(
            uri = "createUpdateAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsCreateUpdateAsset")
        public static String createUpdateAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "copyAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetRedirectResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsCopyAsset")
        public static String copyAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addAttributeToAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsCreateUpdateAttribute")
        public static String addAttributeToAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateAssetAttribute",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsCreateUpdateAttribute")
        public static String updateAssetAttribute(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteAttributeFromAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsDeleteAttribute")
        public static String deleteAttributeFromAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "addScriptToAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsUpdateAssetTemplateScript")
        public static String addScriptToAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateAssetScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsUpdateAssetTemplateScript")
        public static String updateAssetScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteScriptAndAssetTemplateAssoc",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editAssetResponse")
        @Response(name = "error", type = "request", value = "editAssetResponse")
        @Event(type = "service", invoke = "cmsDeleteScriptAndAssetTemplateAssoc")
        public static String deleteScriptAndAssetTemplateAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getAssetTypes",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetAssetTemplateTypes")
        public static String getAssetTypes(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "getAssets",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetAssetTemplates")
        public static String getAssets(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getAssetAttributes",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetAssetTemplateAttributes")
        public static String getAssetAttributes(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateAssetInfo",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsUpdateAssetTemplateInfo")
        public static String updateAssetInfo(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteAsset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsDeleteAssetTemplate")
        public static String deleteAsset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "scripts",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "scripts")
        public interface Scripts {}

        @Request(
            uri = "editScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editScript")
        public interface EditScript {}

        @Request(
            uri = "createUpdateScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editScript")
        @Response(name = "error", type = "view", value = "editScript")
        @Event(type = "service", invoke = "cmsCreateUpdateScriptTemplate")
        public static String createUpdateScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "copyScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editScript")
        @Response(name = "error", type = "view", value = "editScript")
        @Event(type = "service", invoke = "cmsCopyScriptTemplate")
        public static String copyScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateScriptInfo",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsUpdateScriptTemplateInfo")
        public static String updateScriptInfo(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteScript",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsDeleteScriptTemplate")
        public static String deleteScript(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "media",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "media")
        public interface Media {}

        @Request(
            uri = "editMedia",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editMedia")
        public interface EditMedia {}

        @Request(
            uri = "createMedia",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "createMediaUpdateProgress")
        @Response(name = "error", type = "request", value = "createMediaUpdateProgress")
        @Event(type = "service", invoke = "cmsUploadMediaFile")
        public static String createMedia(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "createMediaImageCustomSizes",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "createMediaUpdateProgress")
        @Response(name = "error", type = "request", value = "createMediaUpdateProgress")
        @Event(type = "service", invoke = "cmsUploadMediaFileImageCustomVariantSizes")
        public static String createMediaImageCustomSizes(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "createMediaUpdateProgress",
            controller = "cms",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "editMedia")
        @Response(name = "error", type = "view", value = "editMedia")
        public interface CreateMediaUpdateProgress {}

        @Request(
            uri = "customImageSizePresets",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "customImageSizePresets")
        public interface CustomImageSizePresets {}

        @Request(
            uri = "createCustomImageSizePreset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearImageProfileCaches")
        @Response(name = "error", type = "view", value = "customImageSizePresets")
        @Event(type = "service", invoke = "cmsCreateCustomImageSizePreset")
        public static String createCustomImageSizePreset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "updateCustomImageSizePreset",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearImageProfileCaches")
        @Response(name = "error", type = "view", value = "customImageSizePresets")
        @Event(type = "service", invoke = "cmsUpdateCustomImageSizePreset")
        public static String updateCustomImageSizePreset(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "clearImageProfileCaches",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "customImageSizePresets")
        @Response(name = "error", type = "view", value = "customImageSizePresets")
        public interface ClearImageProfileCaches {}

        @Request(
            uri = "updateMedia",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editMedia")
        @Response(name = "error", type = "view", value = "editMedia")
        @Event(type = "service", invoke = "cmsUpdateMediaFile")
        public static String updateMedia(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "deleteMedia",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "media")
        @Response(name = "error", type = "view", value = "editMedia")
        @Event(type = "service", invoke = "cmsDeleteMediaFile")
        public static String deleteMedia(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "rebuildAllMediaVariants",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "media")
        @Response(name = "error", type = "view", value = "media")
        @Event(type = "service", path = "async", invoke = "cmsRebuildMediaVariants")
        public static String rebuildAllMediaVariants(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "rebuildMediaVariantList",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editMedia")
        @Response(name = "error", type = "view", value = "editMedia")
        @Event(type = "service", invoke = "cmsRebuildMediaVariantList")
        public static String rebuildMediaVariantList(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "removeAllMediaVariants",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "media")
        @Response(name = "error", type = "view", value = "media")
        @Event(type = "service", path = "async", invoke = "cmsRemoveMediaVariants")
        public static String removeAllMediaVariants(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "removeMediaVariants",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "editMedia")
        @Response(name = "error", type = "view", value = "editMedia")
        @Event(type = "service", invoke = "cmsRemoveMediaVariants")
        public static String removeMediaVariants(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getMediaFiles",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetMediaFiles")
        public static String getMediaFiles(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "menus",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "menus")
        public interface Menus {}

        @Request(
            uri = "getMenu",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetMenu")
        public static String getMenu(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "getMenus",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsGetMenus")
        public static String getMenus(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "saveMenu",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsCreateUpdateMenu")
        public static String saveMenu(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "deleteMenu",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsDeleteMenu")
        public static String deleteMenu(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "libmimetypeexport",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "libMimeTypeExport")
        public interface Libmimetypeexport {}

        @Request(
            uri = "CmsDataImport",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CmsDataImport")
        public interface CmsDataImport {}

        @Request(
            uri = "importCmsData",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CmsDataImport")
        @Response(name = "error", type = "view", value = "CmsDataImport")
        @Event(type = "service", invoke = "cmsImportXmlData")
        public static String importCmsData(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "CmsDataExport",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CmsDataExport")
        public interface CmsDataExport {}

        @Request(
            uri = "CmsDataExportRaw.xml",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CmsDataExportRaw")
        public interface CmsDataExportRawXml {}

        @Request(
            uri = "exportCmsDataAsXmlJson",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "cmsExportDataAsXmlInline")
        public static String exportCmsDataAsXmlJson(HttpServletRequest request, HttpServletResponse response) {
            return "error"; // Event dispatched via @Event annotation
        }

        @Request(
            uri = "settings",
            controller = "cms",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "robots")
        public interface Settings {}


    }
}
