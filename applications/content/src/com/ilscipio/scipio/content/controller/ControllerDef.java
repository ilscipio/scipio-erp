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
package com.ilscipio.scipio.content.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.content.layout.LayoutEvents;
import org.ofbiz.content.compdoc.CompDocEvents;
import org.ofbiz.content.data.DataEvents;
import org.ofbiz.content.content.ContentEvents;
import org.ofbiz.content.content.UploadContentAndImage;
import org.ofbiz.webapp.event.TestEvent;

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
        page = "component://content/widget/CommonScreens.xml#main",
        controller = "content"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "fonts.fo",
        type = "screenfop",
        page = "component://content/widget/CommonScreens.xml#fonts.fo",
        contentType = "application/pdf",
        encoding = "none",
        controller = "content"
    )
    public static final String VIEW_FONTS_FO = "fonts.fo";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "blogMain",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#BlogMain",
        controller = "content"
    )
    public static final String VIEW_BLOGMAIN = "blogMain";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "blogContent",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#BlogContent",
        controller = "content"
    )
    public static final String VIEW_BLOGCONTENT = "blogContent";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewBlogArticle",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#ViewArticle",
        controller = "content"
    )
    public static final String VIEW_VIEWBLOGARTICLE = "ViewBlogArticle";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBlog",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#EditBlog",
        controller = "content"
    )
    public static final String VIEW_EDITBLOG = "EditBlog";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBlogArticle",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#EditArticle",
        controller = "content"
    )
    public static final String VIEW_EDITBLOGARTICLE = "EditBlogArticle";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewResponse",
        type = "screen",
        page = "component://content/widget/forum/BlogScreens.xml#BlogMain",
        controller = "content"
    )
    public static final String VIEW_VIEWRESPONSE = "ViewResponse";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "RespondBlog",
        type = "screen",
        page = "component://common/widget/CommonScreens.xml#error",
        controller = "content"
    )
    public static final String VIEW_RESPONDBLOG = "RespondBlog";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBlogText",
        type = "screen",
        page = "component://common/widget/CommonScreens.xml#error",
        controller = "content"
    )
    public static final String VIEW_EDITBLOGTEXT = "EditBlogText";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBlogImage",
        type = "screen",
        page = "component://common/widget/CommonScreens.xml#error",
        controller = "content"
    )
    public static final String VIEW_EDITBLOGIMAGE = "EditBlogImage";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBlogResponse",
        type = "screen",
        page = "component://common/widget/CommonScreens.xml#error",
        controller = "content"
    )
    public static final String VIEW_EDITBLOGRESPONSE = "EditBlogResponse";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LatestResponses",
        type = "screen",
        page = "component://common/widget/CommonScreens.xml#error",
        controller = "content"
    )
    public static final String VIEW_LATESTRESPONSES = "LatestResponses";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindForumGroups",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#FindForumGroups",
        controller = "content"
    )
    public static final String VIEW_FINDFORUMGROUPS = "FindForumGroups";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ForumGroupRoles",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#ForumGroupRoles",
        controller = "content"
    )
    public static final String VIEW_FORUMGROUPROLES = "ForumGroupRoles";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ForumGroupPurposes",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#ForumGroupPurposes",
        controller = "content"
    )
    public static final String VIEW_FORUMGROUPPURPOSES = "ForumGroupPurposes";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindForums",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#FindForums",
        controller = "content"
    )
    public static final String VIEW_FINDFORUMS = "FindForums";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindForumMessages",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#FindForumMessages",
        controller = "content"
    )
    public static final String VIEW_FINDFORUMMESSAGES = "FindForumMessages";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindForumThreads",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#FindForumThreads",
        controller = "content"
    )
    public static final String VIEW_FINDFORUMTHREADS = "FindForumThreads";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "AddForumMessage",
        type = "screen",
        page = "component://content/widget/forum/ForumScreens.xml#AddForumMessage",
        controller = "content"
    )
    public static final String VIEW_ADDFORUMMESSAGE = "AddForumMessage";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddForumThreadMessage",
            type = "screen",
            page = "component://content/widget/forum/ForumScreens.xml#AddForumThreadMessage",
            controller = "content"
        )
        public static final String VIEW_ADDFORUMTHREADMESSAGE = "AddForumThreadMessage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditForumMessage",
            type = "screen",
            page = "component://content/widget/forum/ForumScreens.xml#EditForumMessage",
            controller = "content"
        )
        public static final String VIEW_EDITFORUMMESSAGE = "EditForumMessage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindWebSite",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#FindWebSite",
            controller = "content"
        )
        public static final String VIEW_FINDWEBSITE = "FindWebSite";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWebSite",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#EditWebSite",
            controller = "content"
        )
        public static final String VIEW_EDITWEBSITE = "EditWebSite";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteAliases",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteAliases",
            controller = "content"
        )
        public static final String VIEW_WEBSITEALIASES = "WebSiteAliases";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteAliasesSearchResults",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteAliasesSearchResults",
            controller = "content"
        )
        public static final String VIEW_WEBSITEALIASESSEARCHRESULTS = "WebSiteAliasesSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteContent",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteContent",
            controller = "content"
        )
        public static final String VIEW_WEBSITECONTENT = "WebSiteContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMS",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMS",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMS = "WebSiteCMS";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMSContent",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMSContent",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMSCONTENT = "WebSiteCMSContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMSEditor",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMSEditor",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMSEDITOR = "WebSiteCMSEditor";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMSMetaInfo",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMSMetaInfo",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMSMETAINFO = "WebSiteCMSMetaInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMSPathAlias",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMSPathAlias",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMSPATHALIAS = "WebSiteCMSPathAlias";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteCMSNav",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteCMSNav",
            controller = "content"
        )
        public static final String VIEW_WEBSITECMSNAV = "WebSiteCMSNav";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWebSiteParties",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#EditWebSiteParties",
            controller = "content"
        )
        public static final String VIEW_EDITWEBSITEPARTIES = "EditWebSiteParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteSEO",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteSEO",
            controller = "content"
        )
        public static final String VIEW_WEBSITESEO = "WebSiteSEO";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebSiteContactList",
            type = "screen",
            page = "component://content/widget/WebSiteScreens.xml#WebSiteContactList",
            controller = "content"
        )
        public static final String VIEW_WEBSITECONTACTLIST = "WebSiteContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentPurpose",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentPurpose",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTPURPOSE = "EditContentPurpose";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentRole",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentRole",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTROLE = "EditContentRole";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#FindContent",
            controller = "content"
        )
        public static final String VIEW_FINDCONTENT = "FindContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findContentSearchResults",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#findContentSearchResults",
            controller = "content"
        )
        public static final String VIEW_FINDCONTENTSEARCHRESULTS = "findContentSearchResults";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContent",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENT = "EditContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentAssoc",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentAssoc",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTASSOC = "EditContentAssoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWebSite",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ListWebSite",
            controller = "content"
        )
        public static final String VIEW_LISTWEBSITE = "ListWebSite";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentAttribute",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentAttribute",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTATTRIBUTE = "EditContentAttribute";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentMetaData",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentMetaData",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTMETADATA = "EditContentMetaData";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentType",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentType",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTTYPE = "EditContentType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentAssocType",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentAssocType",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTASSOCTYPE = "EditContentAssocType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentPurposeType",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentPurposeType",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTPURPOSETYPE = "EditContentPurposeType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentTypeAttr",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentTypeAttr",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTTYPEATTR = "EditContentTypeAttr";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentAssocPredicate",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentAssocPredicate",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTASSOCPREDICATE = "EditContentAssocPredicate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentOperation",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentOperation",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTOPERATION = "EditContentOperation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentPurposeOperation",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#EditContentPurposeOperation",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTPURPOSEOPERATION = "EditContentPurposeOperation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentWorkEfforts",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentWorkEfforts",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTWORKEFFORTS = "EditContentWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentKeywords",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditContentKeywords",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTKEYWORDS = "EditContentKeywords";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#FindDataResource",
            controller = "content"
        )
        public static final String VIEW_FINDDATARESOURCE = "FindDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findDataResourceSearchResults",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#findDataResourceSearchResults",
            controller = "content"
        )
        public static final String VIEW_FINDDATARESOURCESEARCHRESULTS = "findDataResourceSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "navigateDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#navigateDataResource",
            controller = "content"
        )
        public static final String VIEW_NAVIGATEDATARESOURCE = "navigateDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "listDataResources",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#listDataResources",
            controller = "content"
        )
        public static final String VIEW_LISTDATARESOURCES = "listDataResources";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UploadImage",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#UploadImage",
            controller = "content"
        )
        public static final String VIEW_UPLOADIMAGE = "UploadImage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResource",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCE = "EditDataResource";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#AddDataResource",
            controller = "content"
        )
        public static final String VIEW_ADDDATARESOURCE = "AddDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddDataResourceText",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#AddDataResourceText",
            controller = "content"
        )
        public static final String VIEW_ADDDATARESOURCETEXT = "AddDataResourceText";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddDataResourceUrl",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#AddDataResourceUrl",
            controller = "content"
        )
        public static final String VIEW_ADDDATARESOURCEURL = "AddDataResourceUrl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddDataResourceUpload",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#AddDataResourceUpload",
            controller = "content"
        )
        public static final String VIEW_ADDDATARESOURCEUPLOAD = "AddDataResourceUpload";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddDataResourceFromContent",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#AddDataResourceFromContent",
            controller = "content"
        )
        public static final String VIEW_ADDDATARESOURCEFROMCONTENT = "AddDataResourceFromContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceText",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceText",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCETEXT = "EditDataResourceText";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceUrl",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceUrl",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCEURL = "EditDataResourceUrl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceUpload",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceUpload",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCEUPLOAD = "EditDataResourceUpload";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditElectronicText",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditElectronicText",
            controller = "content"
        )
        public static final String VIEW_EDITELECTRONICTEXT = "EditElectronicText";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceAttribute",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceAttribute",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCEATTRIBUTE = "EditDataResourceAttribute";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceRole",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceRole",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCEROLE = "EditDataResourceRole";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceProductFeatures",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditDataResourceProductFeatures",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCEPRODUCTFEATURES = "EditDataResourceProductFeatures";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditHtmlText",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#EditHtmlText",
            controller = "content"
        )
        public static final String VIEW_EDITHTMLTEXT = "EditHtmlText";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceType",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditDataResourceType",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCETYPE = "EditDataResourceType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataResourceTypeAttr",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditDataResourceTypeAttr",
            controller = "content"
        )
        public static final String VIEW_EDITDATARESOURCETYPEATTR = "EditDataResourceTypeAttr";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDataCategory",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditDataCategory",
            controller = "content"
        )
        public static final String VIEW_EDITDATACATEGORY = "EditDataCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditMetaDataPredicate",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditMetaDataPredicate",
            controller = "content"
        )
        public static final String VIEW_EDITMETADATAPREDICATE = "EditMetaDataPredicate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCharacterSet",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditCharacterSet",
            controller = "content"
        )
        public static final String VIEW_EDITCHARACTERSET = "EditCharacterSet";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFileExtension",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditFileExtension",
            controller = "content"
        )
        public static final String VIEW_EDITFILEEXTENSION = "EditFileExtension";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditMimeType",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditMimeType",
            controller = "content"
        )
        public static final String VIEW_EDITMIMETYPE = "EditMimeType";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditMimeTypeHtmlTemplate",
            type = "screen",
            page = "component://content/widget/datasetup/DataResourceSetupScreens.xml#EditMimeTypeHtmlTemplate",
            controller = "content"
        )
        public static final String VIEW_EDITMIMETYPEHTMLTEMPLATE = "EditMimeTypeHtmlTemplate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListLayout",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#ListLayout",
            controller = "content"
        )
        public static final String VIEW_LISTLAYOUT = "ListLayout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindLayout",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#FindLayout",
            controller = "content"
        )
        public static final String VIEW_FINDLAYOUT = "FindLayout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayout",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayout",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUT = "EditLayout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddLayout",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#AddLayout",
            controller = "content"
        )
        public static final String VIEW_ADDLAYOUT = "AddLayout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayoutSubContent",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayoutSubContent",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUTSUBCONTENT = "EditLayoutSubContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayoutText",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayoutText",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUTTEXT = "EditLayoutText";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayoutHtml",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayoutHtml",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUTHTML = "EditLayoutHtml";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayoutImage",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayoutImage",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUTIMAGE = "EditLayoutImage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditLayoutUrl",
            type = "screen",
            page = "component://content/widget/layout/LayoutScreens.xml#EditLayoutUrl",
            controller = "content"
        )
        public static final String VIEW_EDITLAYOUTURL = "EditLayoutUrl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSurvey",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#FindSurvey",
            controller = "content"
        )
        public static final String VIEW_FINDSURVEY = "FindSurvey";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFindSurveySearchResults",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#ListFindSurveySearchResults",
            controller = "content"
        )
        public static final String VIEW_LISTFINDSURVEYSEARCHRESULTS = "ListFindSurveySearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSurvey",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#EditSurvey",
            controller = "content"
        )
        public static final String VIEW_EDITSURVEY = "EditSurvey";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSurveyMultiResps",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#EditSurveyMultiResps",
            controller = "content"
        )
        public static final String VIEW_EDITSURVEYMULTIRESPS = "EditSurveyMultiResps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSurveyQuestions",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#EditSurveyQuestions",
            controller = "content"
        )
        public static final String VIEW_EDITSURVEYQUESTIONS = "EditSurveyQuestions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSurveyResponse",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#FindSurveyResponse",
            controller = "content"
        )
        public static final String VIEW_FINDSURVEYRESPONSE = "FindSurveyResponse";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewSurveyResponses",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#ViewSurveyResponses",
            controller = "content"
        )
        public static final String VIEW_VIEWSURVEYRESPONSES = "ViewSurveyResponses";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSurveyResponse",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#EditSurveyResponse",
            controller = "content"
        )
        public static final String VIEW_EDITSURVEYRESPONSE = "EditSurveyResponse";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CMSContentFind",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#CMSContentFind",
            controller = "content"
        )
        public static final String VIEW_CMSCONTENTFIND = "CMSContentFind";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CMSContentEdit",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#CMSContentEdit",
            controller = "content"
        )
        public static final String VIEW_CMSCONTENTEDIT = "CMSContentEdit";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UserPermissions",
            type = "screen",
            page = "component://content/widget/contentsetup/ContentSetupScreens.xml#UserPermissions",
            controller = "content"
        )
        public static final String VIEW_USERPERMISSIONS = "UserPermissions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CMSSites",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#CMSSites",
            controller = "content"
        )
        public static final String VIEW_CMSSITES = "CMSSites";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "addSubSite",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#addSubSite",
            controller = "content"
        )
        public static final String VIEW_ADDSUBSITE = "addSubSite";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAddContent",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#EditAddContent",
            controller = "content"
        )
        public static final String VIEW_EDITADDCONTENT = "EditAddContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAddSubContent",
            type = "screen",
            page = "component://content/widget/cms/CMSScreens.xml#EditAddSubContent",
            controller = "content"
        )
        public static final String VIEW_EDITADDSUBCONTENT = "EditAddSubContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContentRevisions",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ListContentRevisions",
            controller = "content"
        )
        public static final String VIEW_LISTCONTENTREVISIONS = "ListContentRevisions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentRevision",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditContentRevision",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTREVISION = "EditContentRevision";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContentRevisionItem",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ListContentRevisionItem",
            controller = "content"
        )
        public static final String VIEW_LISTCONTENTREVISIONITEM = "ListContentRevisionItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentRevisionItem",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditContentRevisionItem",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTREVISIONITEM = "EditContentRevisionItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContentApproval",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ListContentApproval",
            controller = "content"
        )
        public static final String VIEW_LISTCONTENTAPPROVAL = "ListContentApproval";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListWaitingContentApproval",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ListWaitingContentApproval",
            controller = "content"
        )
        public static final String VIEW_LISTWAITINGCONTENTAPPROVAL = "ListWaitingContentApproval";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditContentApproval",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditContentApproval",
            controller = "content"
        )
        public static final String VIEW_EDITCONTENTAPPROVAL = "EditContentApproval";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindCompDoc",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#FindCompDoc",
            controller = "content"
        )
        public static final String VIEW_FINDCOMPDOC = "FindCompDoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRootCompDoc",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditRootCompDoc",
            controller = "content"
        )
        public static final String VIEW_EDITROOTCOMPDOC = "EditRootCompDoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditChildCompDoc",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditChildCompDoc",
            controller = "content"
        )
        public static final String VIEW_EDITCHILDCOMPDOC = "EditChildCompDoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddChildCompDocInstance",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#AddChildCompDocInstance",
            controller = "content"
        )
        public static final String VIEW_ADDCHILDCOMPDOCINSTANCE = "AddChildCompDocInstance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddChildCompDocTemplate",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#AddChildCompDocTemplate",
            controller = "content"
        )
        public static final String VIEW_ADDCHILDCOMPDOCTEMPLATE = "AddChildCompDocTemplate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddRootCompDocInstance",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#AddRootCompDocInstance",
            controller = "content"
        )
        public static final String VIEW_ADDROOTCOMPDOCINSTANCE = "AddRootCompDocInstance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddRootCompDocTemplate",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#AddRootCompDocTemplate",
            controller = "content"
        )
        public static final String VIEW_ADDROOTCOMPDOCTEMPLATE = "AddRootCompDocTemplate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewCompDocTemplateTree",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ViewCompDocTemplateTree",
            controller = "content"
        )
        public static final String VIEW_VIEWCOMPDOCTEMPLATETREE = "ViewCompDocTemplateTree";

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewCompDocInstanceTree",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ViewCompDocInstanceTree",
            controller = "content"
        )
        public static final String VIEW_VIEWCOMPDOCINSTANCETREE = "ViewCompDocInstanceTree";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCompDocContentRole",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#EditCompDocContentRole",
            controller = "content"
        )
        public static final String VIEW_EDITCOMPDOCCONTENTROLE = "EditCompDocContentRole";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewInstances",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ViewInstances",
            controller = "content"
        )
        public static final String VIEW_VIEWINSTANCES = "ViewInstances";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewCompDocContentBinary",
            type = "simplecontent",
            controller = "content"
        )
        public static final String VIEW_VIEWCOMPDOCCONTENTBINARY = "ViewCompDocContentBinary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewBinaryDataResource",
            type = "simplecontent",
            controller = "content"
        )
        public static final String VIEW_VIEWBINARYDATARESOURCE = "ViewBinaryDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewCompDocContent",
            type = "screen",
            page = "component://content/widget/compdoc/CompDocScreens.xml#ViewCompDocContent",
            controller = "content"
        )
        public static final String VIEW_VIEWCOMPDOCCONTENT = "ViewCompDocContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewSimpleContent",
            type = "simplecontent",
            controller = "content"
        )
        public static final String VIEW_VIEWSIMPLECONTENT = "ViewSimpleContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "content"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupTreeContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContentTree",
            controller = "content"
        )
        public static final String VIEW_LOOKUPTREECONTENT = "LookupTreeContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupDetailContentTree",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupDetailContentTree",
            controller = "content"
        )
        public static final String VIEW_LOOKUPDETAILCONTENTTREE = "LookupDetailContentTree";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#LookupDataResource",
            controller = "content"
        )
        public static final String VIEW_LOOKUPDATARESOURCE = "LookupDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSurvey",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#LookupSurvey",
            controller = "content"
        )
        public static final String VIEW_LOOKUPSURVEY = "LookupSurvey";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSurveyResponse",
            type = "screen",
            page = "component://content/widget/SurveyScreens.xml#LookupSurveyResponse",
            controller = "content"
        )
        public static final String VIEW_LOOKUPSURVEYRESPONSE = "LookupSurveyResponse";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupListLayout",
            type = "screen",
            page = "component://content/widget/LookupScreens.xml#LookupListLayout",
            controller = "content"
        )
        public static final String VIEW_LOOKUPLISTLAYOUT = "LookupListLayout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSubContent",
            type = "screen",
            page = "component://content/widget/LookupScreens.xml#LookupSubContent",
            controller = "content"
        )
        public static final String VIEW_LOOKUPSUBCONTENT = "LookupSubContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupUserLoginAndPartyDetails",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
            controller = "content"
        )
        public static final String VIEW_LOOKUPUSERLOGINANDPARTYDETAILS = "LookupUserLoginAndPartyDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "content"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyAndUserLoginAndPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyAndUserLoginAndPerson",
            controller = "content"
        )
        public static final String VIEW_LOOKUPPARTYANDUSERLOGINANDPERSON = "LookupPartyAndUserLoginAndPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "content"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "content"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "content"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "navigateContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#navigateContent",
            controller = "content"
        )
        public static final String VIEW_NAVIGATECONTENT = "navigateContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDocumentTree",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditDocumentTree",
            controller = "content"
        )
        public static final String VIEW_EDITDOCUMENTTREE = "EditDocumentTree";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDocument",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#EditDocument",
            controller = "content"
        )
        public static final String VIEW_EDITDOCUMENT = "EditDocument";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListDocument",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ListDocument",
            controller = "content"
        )
        public static final String VIEW_LISTDOCUMENT = "ListDocument";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListContentTree",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ListContentTree",
            controller = "content"
        )
        public static final String VIEW_LISTCONTENTTREE = "ListContentTree";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewContentDetail",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ViewContentDetail",
            controller = "content"
        )
        public static final String VIEW_VIEWCONTENTDETAIL = "ViewContentDetail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ShowContent",
            controller = "content"
        )
        public static final String VIEW_SHOWCONTENT = "showContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showContentPdf",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ShowContent",
            contentType = "application/pdf",
            encoding = "none",
            controller = "content"
        )
        public static final String VIEW_SHOWCONTENTPDF = "showContentPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ContentSearchOptions",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ContentSearchOptions",
            controller = "content"
        )
        public static final String VIEW_CONTENTSEARCHOPTIONS = "ContentSearchOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ContentSearchResults",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#ContentSearchResults",
            controller = "content"
        )
        public static final String VIEW_CONTENTSEARCHRESULTS = "ContentSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindWebAnalyticsConfigs",
            type = "screen",
            page = "component://content/widget/WebAnalyticsScreens.xml#FindWebAnalyticsConfigs",
            controller = "content"
        )
        public static final String VIEW_FINDWEBANALYTICSCONFIGS = "FindWebAnalyticsConfigs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditWebAnalyticsConfig",
            type = "screen",
            page = "component://content/widget/WebAnalyticsScreens.xml#EditWebAnalyticsConfig",
            controller = "content"
        )
        public static final String VIEW_EDITWEBANALYTICSCONFIG = "EditWebAnalyticsConfig";

        @Request(
            uri = "chain",
            controller = "content",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "/view")
        @Response(name = "error", type = "view", value = "error")
        public static String chain(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.TestEvent.test
            return TestEvent.test(request, response);
        }

        @Request(
            uri = "main",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWebSite")
        public interface Main {}

        @Request(
            uri = "fonts.pdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "fonts.fo")
        public interface FontsPdf {}

        @Request(
            uri = "blogMain",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "blogMain")
        public interface BlogMain {}

        @Request(
            uri = "editBlog",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlog")
        public interface EditBlog {}

        @Request(
            uri = "updateBlog",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "blogMain")
        @Response(name = "error", type = "view", value = "EditBlog")
        @Event(type = "service", invoke = "updateContent")
        public static String updateBlog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "newBlog",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "blogMain")
        @Response(name = "error", type = "view", value = "EditBlog")
        @Event(type = "service", invoke = "createContent")
        public static String newBlog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "blogContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "blogContent")
        public interface BlogContent {}

        @Request(
            uri = "updateBlogArticle",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "blogContent")
        @Response(name = "error", type = "view", value = "EditBlogArticle")
        @Event(type = "service", invoke = "updateBlogEntry")
        public static String updateBlogArticle(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBlogArticle",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "EditBlogArticle")
        @Response(name = "success", type = "view", value = "blogContent")
        @Event(type = "service", invoke = "createBlogEntry")
        public static String createBlogArticle(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewBlogArticle",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewBlogArticle")
        public interface ViewBlogArticle {}

        @Request(
            uri = "ViewBlogRss",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "error")
        @Event(type = "rome", invoke = "generateBlogRssFeed")
        public static String viewBlogRss(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewResponse")
        public interface ViewResponse {}

        @Request(
            uri = "LatestResponses",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LatestResponses")
        public interface LatestResponses {}

        @Request(
            uri = "EditBlogArticle",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlogArticle")
        public interface EditBlogArticle {}

        @Request(
            uri = "EditBlogImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlogImage")
        public interface EditBlogImage {}

        @Request(
            uri = "EditBlogText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlogText")
        public interface EditBlogText {}

        @Request(
            uri = "RespondBlog",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RespondBlog")
        public interface RespondBlog {}

        @Request(
            uri = "persistBlogSummary",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlog")
        @Response(name = "error", type = "view", value = "EditBlog")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistBlogSummary(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "persistBlogText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlog")
        @Response(name = "error", type = "view", value = "EditBlog")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistBlogText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "persistBlogImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBlog")
        @Response(name = "error", type = "view", value = "EditBlog")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistBlogImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBlogResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewResponse")
        @Response(name = "error", type = "view", value = "ViewResponse")
        @Event(type = "service", invoke = "createTextContent")
        public static String createBlogResponse(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBlogResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewResponse")
        @Response(name = "error", type = "view", value = "ViewResponse")
        @Event(type = "service", invoke = "updateTextContent")
        public static String updateBlogResponse(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findForumGroups",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumGroups")
        public interface FindForumGroups {}

        @Request(
            uri = "createForumGroup",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumGroups")
        @Response(name = "error", type = "view", value = "FindForumGroups")
        @Event(type = "service", invoke = "createContent")
        public static String createForumGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateForumGroup",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumGroups")
        @Response(name = "error", type = "view", value = "FindForumGroups")
        @Event(type = "service", invoke = "updateContent")
        public static String updateForumGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "forumGroupRoles",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupRoles")
        public interface ForumGroupRoles {}

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "createForumGroupRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupRoles")
        @Response(name = "error", type = "view", value = "ForumGroupRoles")
        @Event(type = "service", invoke = "createContentRole")
        public static String createForumGroupRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateForumGroupRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupRoles")
        @Response(name = "error", type = "view", value = "ForumGroupRoles")
        @Event(type = "service", invoke = "updateContentRole")
        public static String updateForumGroupRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteForumGroupRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupRoles")
        @Response(name = "error", type = "view", value = "ForumGroupRoles")
        @Event(type = "service", invoke = "removeContentRole")
        public static String deleteForumGroupRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "forumGroupPurposes",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupPurposes")
        public interface ForumGroupPurposes {}

        @Request(
            uri = "createForumGroupPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupPurposes")
        @Response(name = "error", type = "view", value = "ForumGroupPurposes")
        @Event(type = "service", invoke = "createContentPurpose")
        public static String createForumGroupPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteForumGroupPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ForumGroupPurposes")
        @Response(name = "error", type = "view", value = "ForumGroupPurposes")
        @Event(type = "service", invoke = "removeContentPurpose")
        public static String deleteForumGroupPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findForums",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForums")
        public interface FindForums {}

        @Request(
            uri = "createForum",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForums")
        @Response(name = "error", type = "view", value = "FindForums")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String createForum(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateForum",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForums")
        @Response(name = "error", type = "view", value = "FindForums")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String updateForum(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findForumMessages",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumMessages")
        public interface FindForumMessages {}

        @Request(
            uri = "findForumThreads",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumThreads")
        public interface FindForumThreads {}

        @Request(
            uri = "addForumMessage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddForumMessage")
        public interface AddForumMessage {}

        @Request(
            uri = "addForumThreadMessage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddForumThreadMessage")
        public interface AddForumThreadMessage {}

        @Request(
            uri = "editForumMessage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditForumMessage")
        public interface EditForumMessage {}

        @Request(
            uri = "updateForumMessage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumMessages")
        @Response(name = "error", type = "view", value = "FindForumMessages")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String updateForumMessage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateForumThreadMessage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindForumThreads")
        @Response(name = "error", type = "view", value = "FindForumThreads")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String updateForumThreadMessage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindWebSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWebSite")
        public interface FindWebSite {}

        @Request(
            uri = "ListWebSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWebSite")
        public interface ListWebSite {}

        @Request(
            uri = "EditWebSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        public interface EditWebSite {}

        @Request(
            uri = "createWebSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        @Response(name = "error", type = "view", value = "EditWebSite")
        @Event(type = "service", invoke = "createWebSite")
        public static String createWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "updateWebSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        @Response(name = "error", type = "view", value = "EditWebSite")
        @Event(type = "service", invoke = "updateWebSite")
        public static String updateWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListWebSiteContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContent")
        public interface ListWebSiteContent {}

        @Request(
            uri = "CreateWebSiteContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContent")
        @Response(name = "error", type = "view", value = "WebSiteContent")
        @Event(type = "service", invoke = "createWebSiteContent")
        public static String createWebSiteContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateWebSiteContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContent")
        @Response(name = "error", type = "view", value = "WebSiteContent")
        @Event(type = "service", invoke = "updateWebSiteContent")
        public static String updateWebSiteContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveWebSiteContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContent")
        @Response(name = "error", type = "view", value = "WebSiteContent")
        @Event(type = "service", invoke = "removeWebSiteContent")
        public static String removeWebSiteContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "autoCreateWebSiteContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContent")
        @Response(name = "error", type = "view", value = "WebSiteContent")
        @Event(type = "service", invoke = "autoCreateWebSiteContent")
        public static String autoCreateWebSiteContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWebSiteParties",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSiteParties")
        public interface EditWebSiteParties {}

        @Request(
            uri = "createWebSiteRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSiteParties")
        @Response(name = "error", type = "view", value = "EditWebSiteParties")
        @Event(type = "service", invoke = "createWebSiteRole")
        public static String createWebSiteRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWebSiteRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSiteParties")
        @Response(name = "error", type = "view", value = "EditWebSiteParties")
        @Event(type = "service", invoke = "updateWebSiteRole")
        public static String updateWebSiteRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWebSiteRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSiteParties")
        @Response(name = "error", type = "view", value = "EditWebSiteParties")
        @Event(type = "service", invoke = "removeWebSiteRole")
        public static String removeWebSiteRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "WebSiteAliases",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteAliases")
        public interface WebSiteAliases {}

        @Request(
            uri = "WebSiteAliasesSearchResults",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteAliasesSearchResults")
        public interface WebSiteAliasesSearchResults {}

        @Request(
            uri = "WebSiteCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        public interface WebSiteCms {}

        @Request(
            uri = "createTextContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "createTextContent")
        public static String createTextContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTextContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "updateTextContent")
        public static String updateTextContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createObjectContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "createContentFromUploadedFile")
        public static String createObjectContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateObjectContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "updateContentAndUploadedFile")
        public static String updateObjectContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "createContent")
        public static String createContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentCms",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteCMS")
        @Response(name = "error", type = "view", value = "WebSiteCMS")
        @Event(type = "service", invoke = "updateContent")
        public static String updateContentCms(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWebSiteMetaInfoJson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createTextContent")
        public static String createWebSiteMetaInfoJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "updateWebSiteMetaInfoJson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "updateDataResource")
        public static String updateWebSiteMetaInfoJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWebSitePathAliasJson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createWebSitePathAlias")
        public static String createWebSitePathAliasJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWebSitePathAliasJson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "removeWebSitePathAlias")
        public static String removeWebSitePathAliasJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentType")
        public interface EditContentType {}

        @Request(
            uri = "addContentType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentType")
        @Response(name = "error", type = "view", value = "EditContentType")
        @Event(type = "service", invoke = "createContentType")
        public static String addContentType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentType")
        @Response(name = "error", type = "view", value = "EditContentType")
        @Event(type = "service", invoke = "updateContentType")
        public static String updateContentType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentType")
        @Response(name = "error", type = "view", value = "EditContentType")
        @Event(type = "service", invoke = "removeContentType")
        public static String removeContentType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRole")
        public interface EditContentRole {}

        @Request(
            uri = "addContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRole")
        @Response(name = "error", type = "view", value = "EditContentRole")
        @Event(type = "service", invoke = "createContentRole")
        public static String addContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRole")
        @Response(name = "error", type = "view", value = "EditContentRole")
        @Event(type = "service", invoke = "updateContentRole")
        public static String updateContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRole")
        @Response(name = "error", type = "view", value = "EditContentRole")
        @Event(type = "service", invoke = "removeContentRole")
        public static String removeContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentAssocType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocType")
        public interface EditContentAssocType {}

        @Request(
            uri = "addContentAssocType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocType")
        @Response(name = "error", type = "view", value = "EditContentAssocType")
        @Event(type = "service", invoke = "createContentAssocType")
        public static String addContentAssocType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentAssocType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocType")
        @Response(name = "error", type = "view", value = "EditContentAssocType")
        @Event(type = "service", invoke = "updateContentAssocType")
        public static String updateContentAssocType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentAssocType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocType")
        @Response(name = "error", type = "view", value = "EditContentAssocType")
        @Event(type = "service", invoke = "removeContentAssocType")
        public static String removeContentAssocType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentPurposeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeType")
        public interface EditContentPurposeType {}

        @Request(
            uri = "addContentPurposeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeType")
        @Response(name = "error", type = "view", value = "EditContentPurposeType")
        @Event(type = "service", invoke = "createContentPurposeType")
        public static String addContentPurposeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentPurposeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeType")
        @Response(name = "error", type = "view", value = "EditContentPurposeType")
        @Event(type = "service", invoke = "updateContentPurposeType")
        public static String updateContentPurposeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentPurposeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeType")
        @Response(name = "error", type = "view", value = "EditContentPurposeType")
        @Event(type = "service", invoke = "removeContentPurposeType")
        public static String removeContentPurposeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentTypeAttr")
        public interface EditContentTypeAttr {}

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "addContentTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentTypeAttr")
        @Response(name = "error", type = "view", value = "EditContentTypeAttr")
        @Event(type = "service", invoke = "createContentTypeAttr")
        public static String addContentTypeAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentTypeAttr")
        @Response(name = "error", type = "view", value = "EditContentTypeAttr")
        @Event(type = "service", invoke = "removeContentTypeAttr")
        public static String removeContentTypeAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentAssocPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocPredicate")
        public interface EditContentAssocPredicate {}

        @Request(
            uri = "addContentAssocPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocPredicate")
        @Response(name = "error", type = "view", value = "EditContentAssocPredicate")
        @Event(type = "service", invoke = "createContentAssocPredicate")
        public static String addContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentAssocPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocPredicate")
        @Response(name = "error", type = "view", value = "EditContentAssocPredicate")
        @Event(type = "service", invoke = "updateContentAssocPredicate")
        public static String updateContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentAssocPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssocPredicate")
        @Response(name = "error", type = "view", value = "EditContentAssocPredicate")
        @Event(type = "service", invoke = "removeContentAssocPredicate")
        public static String removeContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCharacterSet",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCharacterSet")
        public interface EditCharacterSet {}

        @Request(
            uri = "addCharacterSet",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCharacterSet")
        @Response(name = "error", type = "view", value = "EditCharacterSet")
        @Event(type = "service", invoke = "createCharacterSet")
        public static String addCharacterSet(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCharacterSet",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCharacterSet")
        @Response(name = "error", type = "view", value = "EditCharacterSet")
        @Event(type = "service", invoke = "updateCharacterSet")
        public static String updateCharacterSet(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeCharacterSet",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCharacterSet")
        @Response(name = "error", type = "view", value = "EditCharacterSet")
        @Event(type = "service", invoke = "removeCharacterSet")
        public static String removeCharacterSet(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataCategory",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataCategory")
        public interface EditDataCategory {}

        @Request(
            uri = "addDataCategory",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataCategory")
        @Response(name = "error", type = "view", value = "EditDataCategory")
        @Event(type = "service", invoke = "createDataCategory")
        public static String addDataCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataCategory",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataCategory")
        @Response(name = "error", type = "view", value = "EditDataCategory")
        @Event(type = "service", invoke = "updateDataCategory")
        public static String updateDataCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDataCategory",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataCategory")
        @Response(name = "error", type = "view", value = "EditDataCategory")
        @Event(type = "service", invoke = "removeDataCategory")
        public static String removeDataCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDataResource")
        public interface FindDataResource {}

        @Request(
            uri = "findDataResourceSearchResults",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findDataResourceSearchResults")
        public interface FindDataResourceSearchResults {}

        @Request(
            uri = "navigateDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "navigateDataResource")
        public interface NavigateDataResource {}

        @Request(
            uri = "listDataResources",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listDataResources")
        public interface ListDataResources {}

        @Request(
            uri = "ViewBinaryDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewBinaryDataResource")
        public interface ViewBinaryDataResource {}

        @Request(
            uri = "EditDataResourceType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceType")
        public interface EditDataResourceType {}

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "addDataResourceType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceType")
        @Response(name = "error", type = "view", value = "EditDataResourceType")
        @Event(type = "service", invoke = "createDataResourceType")
        public static String addDataResourceType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataResourceType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceType")
        @Response(name = "error", type = "view", value = "EditDataResourceType")
        @Event(type = "service", invoke = "updateDataResourceType")
        public static String updateDataResourceType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDataResourceType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceType")
        @Response(name = "error", type = "view", value = "EditDataResourceType")
        @Event(type = "service", invoke = "removeDataResourceType")
        public static String removeDataResourceType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResourceAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceAttribute")
        public interface EditDataResourceAttribute {}

        @Request(
            uri = "addDataResourceAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceAttribute")
        @Response(name = "error", type = "view", value = "EditDataResourceAttribute")
        @Event(type = "service", invoke = "createDataResourceAttribute")
        public static String addDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataResourceAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceAttribute")
        @Response(name = "error", type = "view", value = "EditDataResourceAttribute")
        @Event(type = "service", invoke = "updateDataResourceAttribute")
        public static String updateDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDataResourceAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceAttribute")
        @Response(name = "error", type = "view", value = "EditDataResourceAttribute")
        @Event(type = "service", invoke = "removeDataResourceAttribute")
        public static String removeDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditElectronicText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditElectronicText")
        public interface EditElectronicText {}

        @Request(
            uri = "EditHtmlText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditHtmlText")
        public interface EditHtmlText {}

        @Request(
            uri = "addElectronicText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditElectronicText")
        @Response(name = "error", type = "view", value = "EditElectronicText")
        @Event(type = "service", invoke = "createElectronicText")
        public static String addElectronicText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateElectronicText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditElectronicText")
        @Response(name = "error", type = "view", value = "EditElectronicText")
        @Event(type = "service", invoke = "updateElectronicText")
        public static String updateElectronicText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addHtmlText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditHtmlText")
        @Response(name = "error", type = "view", value = "EditHtmlText")
        @Event(type = "service", invoke = "createElectronicText")
        public static String addHtmlText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateHtmlText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditHtmlText")
        @Response(name = "error", type = "view", value = "EditHtmlText")
        @Event(type = "service", invoke = "updateElectronicText")
        public static String updateHtmlText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeElectronicText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditElectronicText")
        @Response(name = "error", type = "view", value = "EditElectronicText")
        @Event(type = "service", invoke = "removeElectronicText")
        public static String removeElectronicText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResourceRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceRole")
        public interface EditDataResourceRole {}

        @Request(
            uri = "addDataResourceRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceRole")
        @Response(name = "error", type = "view", value = "EditDataResourceRole")
        @Event(type = "service", invoke = "createDataResourceRole")
        public static String addDataResourceRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDataResourceRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceRole")
        @Response(name = "error", type = "view", value = "EditDataResourceRole")
        @Event(type = "service", invoke = "updateDataResourceRole")
        public static String updateDataResourceRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDataResourceRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceRole")
        @Response(name = "error", type = "view", value = "EditDataResourceRole")
        @Event(type = "service", invoke = "removeDataResourceRole")
        public static String removeDataResourceRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResourceProductFeatures",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceProductFeatures")
        public interface EditDataResourceProductFeatures {}

        @Request(
            uri = "createDataResourceProductFeature",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceProductFeatures")
        @Response(name = "error", type = "view", value = "EditDataResourceProductFeatures")
        @Event(type = "service", invoke = "createProductFeatureDataResource")
        public static String createDataResourceProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "removeDataResourceProductFeature",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceProductFeatures")
        @Response(name = "error", type = "view", value = "EditDataResourceProductFeatures")
        @Event(type = "service", invoke = "removeProductFeatureDataResource")
        public static String removeDataResourceProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResourceTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceTypeAttr")
        public interface EditDataResourceTypeAttr {}

        @Request(
            uri = "addDataResourceTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceTypeAttr")
        @Response(name = "error", type = "view", value = "EditDataResourceTypeAttr")
        @Event(type = "service", invoke = "createDataResourceTypeAttr")
        public static String addDataResourceTypeAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDataResourceTypeAttr",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceTypeAttr")
        @Response(name = "error", type = "view", value = "EditDataResourceTypeAttr")
        @Event(type = "service", invoke = "removeDataResourceTypeAttr")
        public static String removeDataResourceTypeAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFileExtension",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFileExtension")
        public interface EditFileExtension {}

        @Request(
            uri = "addFileExtension",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFileExtension")
        @Response(name = "error", type = "view", value = "EditFileExtension")
        @Event(type = "service", invoke = "createFileExtension")
        public static String addFileExtension(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFileExtension",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFileExtension")
        @Response(name = "error", type = "view", value = "EditFileExtension")
        @Event(type = "service", invoke = "updateFileExtension")
        public static String updateFileExtension(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFileExtension",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFileExtension")
        @Response(name = "error", type = "view", value = "EditFileExtension")
        @Event(type = "service", invoke = "removeFileExtension")
        public static String removeFileExtension(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditMetaDataPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMetaDataPredicate")
        public interface EditMetaDataPredicate {}

        @Request(
            uri = "addMetaDataPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMetaDataPredicate")
        @Response(name = "error", type = "view", value = "EditMetaDataPredicate")
        @Event(type = "service", invoke = "createMetaDataPredicate")
        public static String addMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateMetaDataPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMetaDataPredicate")
        @Response(name = "error", type = "view", value = "EditMetaDataPredicate")
        @Event(type = "service", invoke = "updateMetaDataPredicate")
        public static String updateMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeMetaDataPredicate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMetaDataPredicate")
        @Response(name = "error", type = "view", value = "EditMetaDataPredicate")
        @Event(type = "service", invoke = "removeMetaDataPredicate")
        public static String removeMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditMimeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeType")
        public interface EditMimeType {}

        @Request(
            uri = "addMimeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeType")
        @Response(name = "error", type = "view", value = "EditMimeType")
        @Event(type = "service", invoke = "createMimeType")
        public static String addMimeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateMimeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeType")
        @Response(name = "error", type = "view", value = "EditMimeType")
        @Event(type = "service", invoke = "updateMimeType")
        public static String updateMimeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeMimeType",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeType")
        @Response(name = "error", type = "view", value = "EditMimeType")
        @Event(type = "service", invoke = "removeMimeType")
        public static String removeMimeType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditMimeTypeHtmlTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeTypeHtmlTemplate")
        public interface EditMimeTypeHtmlTemplate {}

        @Request(
            uri = "createMimeTypeHtmlTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Response(name = "error", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Event(type = "service", invoke = "createMimeTypeHtmlTemplate")
        public static String createMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateMimeTypeHtmlTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Response(name = "error", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Event(type = "service", invoke = "updateMimeTypeHtmlTemplate")
        public static String updateMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeMimeTypeHtmlTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Response(name = "error", type = "view", value = "EditMimeTypeHtmlTemplate")
        @Event(type = "service", invoke = "removeMimeTypeHtmlTemplate")
        public static String removeMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "findContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContent")
        public interface FindContent {}

        @Request(
            uri = "findContentSearchResults",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findContentSearchResults")
        public interface FindContentSearchResults {}

        @Request(
            uri = "ContentSetupMenu",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentType")
        public interface ContentSetupMenu {}

        @Request(
            uri = "DataSetupMenu",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceType")
        public interface DataSetupMenu {}

        @Request(
            uri = "LayoutMenu",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListLayout")
        public interface LayoutMenu {}

        @Request(
            uri = "EditContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContent")
        public interface EditContent {}

        @Request(
            uri = "updateContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContent")
        @Response(name = "error", type = "view", value = "EditContent")
        @Event(type = "service", invoke = "updateContent")
        public static String updateContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContent")
        @Response(name = "error", type = "view", value = "EditContent")
        @Event(type = "service", invoke = "createContent")
        public static String createContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContent")
        public interface EditContent1 {}

        @Request(
            uri = "EditContentAssoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssoc")
        public interface EditContentAssoc {}

        @Request(
            uri = "updateContentAssoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssoc")
        @Response(name = "error", type = "view", value = "EditContentAssoc")
        @Event(type = "service", invoke = "updateContentAssoc")
        public static String updateContentAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createContentAssoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssoc")
        @Response(name = "error", type = "view", value = "EditContentAssoc")
        @Event(type = "service", invoke = "createContentAssoc")
        public static String createContentAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentAssoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAssoc")
        @Response(name = "error", type = "view", value = "EditContentAssoc")
        @Event(type = "service", invoke = "removeContentAssoc")
        public static String removeContentAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResource")
        public interface EditDataResource {}

        @Request(
            uri = "AddDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddDataResource")
        public interface AddDataResource {}

        @Request(
            uri = "AddDataResourceText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddDataResourceText")
        public interface AddDataResourceText {}

        @Request(
            uri = "AddDataResourceUrl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceUrl")
        public interface AddDataResourceUrl {}

        @Request(
            uri = "AddDataResourceUpload",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddDataResourceUpload")
        public interface AddDataResourceUpload {}

        @Request(
            uri = "AddDataResourceFromContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddDataResourceFromContent")
        public interface AddDataResourceFromContent {}

        @Request(
            uri = "updateDataResourceText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditDataResource")
        @Response(name = "error", type = "view", value = "EditDataResource")
        @Event(type = "service", invoke = "updateDataResource")
        public static String updateDataResourceText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "updateDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResource")
        @Response(name = "error", type = "view", value = "EditDataResource")
        @Event(type = "service", invoke = "updateDataResource")
        public static String updateDataResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResource")
        @Response(name = "ELECTRONIC_TEXT", type = "view", value = "EditElectronicText")
        @Response(name = "IMAGE_OBJECT", type = "view", value = "UploadImage")
        @Response(name = "error", type = "view", value = "AddDataResource")
        public static String createDataResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.data.DataEvents.persistDataResource
            return DataEvents.persistDataResource(request, response);
        }

        @Request(
            uri = "createDataResourceAndAssocToContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContent")
        @Response(name = "ELECTRONIC_TEXT", type = "view", value = "EditElectronicText")
        @Response(name = "IMAGE_OBJECT", type = "view", value = "UploadImage")
        @Response(name = "error", type = "view", value = "AddDataResourceFromContent")
        @Event(type = "service", invoke = "createDataResourceAndAssocToContent")
        public static String createDataResourceAndAssocToContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createDataResourceUpload",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditDataResourceUpload")
        @Response(name = "error", type = "view", value = "AddDataResourceUpload")
        @Event(type = "service", invoke = "createDataResource")
        public static String createDataResourceUpload(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createDataResourceAndText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceText")
        @Response(name = "error", type = "view", value = "AddDataResourceText")
        @Event(type = "service", invoke = "createDataResourceAndText")
        public static String createDataResourceAndText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDataResourceText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceText")
        public interface EditDataResourceText {}

        @Request(
            uri = "EditDataResourceUrl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceUrl")
        public interface EditDataResourceUrl {}

        @Request(
            uri = "EditDataResourceUpload",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDataResourceUpload")
        public interface EditDataResourceUpload {}

        @Request(
            uri = "uploadImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "UploadImage")
        @Response(name = "error", type = "view", value = "UploadImage")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String uploadImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UploadImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UploadImage")
        public interface UploadImage {}

        @Request(
            uri = "img",
            controller = "content"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "request", value = "main")
        public static String img(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.data.DataEvents.serveImage
            return DataEvents.serveImage(request, response);
        }

        @Request(
            uri = "EditContentOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentOperation")
        public interface EditContentOperation {}

        @Request(
            uri = "addContentOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentOperation")
        @Response(name = "error", type = "view", value = "EditContentOperation")
        @Event(type = "service", invoke = "createContentOperation")
        public static String addContentOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentOperation")
        @Response(name = "error", type = "view", value = "EditContentOperation")
        @Event(type = "service", invoke = "updateContentOperation")
        public static String updateContentOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentOperation")
        @Response(name = "error", type = "view", value = "EditContentOperation")
        @Event(type = "service", invoke = "removeContentOperation")
        public static String removeContentOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAttribute")
        public interface EditContentAttribute {}

        @Request(
            uri = "addContentAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAttribute")
        @Response(name = "error", type = "view", value = "EditContentAttribute")
        @Event(type = "service", invoke = "createContentAttribute")
        public static String addContentAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAttribute")
        @Response(name = "error", type = "view", value = "EditContentAttribute")
        @Event(type = "service", invoke = "updateContentAttribute")
        public static String updateContentAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentAttribute",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentAttribute")
        @Response(name = "error", type = "view", value = "EditContentAttribute")
        @Event(type = "service", invoke = "removeContentAttribute")
        public static String removeContentAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentMetaData",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentMetaData")
        public interface EditContentMetaData {}

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "addContentMetaData",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentMetaData")
        @Response(name = "error", type = "view", value = "EditContentMetaData")
        @Event(type = "service", invoke = "createContentMetaData")
        public static String addContentMetaData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentMetaData",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentMetaData")
        @Response(name = "error", type = "view", value = "EditContentMetaData")
        @Event(type = "service", invoke = "updateContentMetaData")
        public static String updateContentMetaData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentMetaData",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentMetaData")
        @Response(name = "error", type = "view", value = "EditContentMetaData")
        @Event(type = "service", invoke = "removeContentMetaData")
        public static String removeContentMetaData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurpose")
        public interface EditContentPurpose {}

        @Request(
            uri = "addContentPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurpose")
        @Response(name = "error", type = "view", value = "EditContentPurpose")
        @Event(type = "service", invoke = "createContentPurpose")
        public static String addContentPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurpose")
        @Response(name = "error", type = "view", value = "EditContentPurpose")
        @Event(type = "service", invoke = "updateContentPurpose")
        public static String updateContentPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentPurpose",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurpose")
        @Response(name = "error", type = "view", value = "EditContentPurpose")
        @Event(type = "service", invoke = "removeContentPurpose")
        public static String removeContentPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentPurposeOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeOperation")
        public interface EditContentPurposeOperation {}

        @Request(
            uri = "addContentPurposeOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeOperation")
        @Response(name = "error", type = "view", value = "EditContentPurposeOperation")
        @Event(type = "service", invoke = "createContentPurposeOperation")
        public static String addContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentPurposeOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeOperation")
        @Response(name = "error", type = "view", value = "EditContentPurposeOperation")
        @Event(type = "service", invoke = "updateContentPurposeOperation")
        public static String updateContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentPurposeOperation",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentPurposeOperation")
        @Response(name = "error", type = "view", value = "EditContentPurposeOperation")
        @Event(type = "service", invoke = "removeContentPurposeOperation")
        public static String removeContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditContentWorkEfforts",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentWorkEfforts")
        public interface EditContentWorkEfforts {}

        @Request(
            uri = "createWorkEffortContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentWorkEfforts")
        @Response(name = "error", type = "view", value = "EditContentWorkEfforts")
        @Event(type = "service", invoke = "createWorkEffortContent")
        public static String createWorkEffortContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentWorkEfforts")
        @Response(name = "error", type = "view", value = "EditContentWorkEfforts")
        @Event(type = "service", invoke = "updateWorkEffortContent")
        public static String updateWorkEffortContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWorkEffortContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentWorkEfforts")
        @Response(name = "error", type = "view", value = "EditContentWorkEfforts")
        @Event(type = "service", invoke = "deleteWorkEffortContent")
        public static String deleteWorkEffortContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindCompDoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCompDoc")
        public interface FindCompDoc {}

        @Request(
            uri = "EditRootCompDoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRootCompDoc")
        public interface EditRootCompDoc {}

        @Request(
            uri = "EditChildCompDoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        public interface EditChildCompDoc {}

        @Request(
            uri = "ViewCompDocTemplateTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocTemplateTree")
        public interface ViewCompDocTemplateTree {}

        @Request(
            uri = "ViewCompDocInstanceTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocInstanceTree")
        public interface ViewCompDocInstanceTree {}

    }

    // Auto-generated split (Part 19)
    public static class Part19 {
        @Request(
            uri = "AddRootCompDocInstance",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddRootCompDocInstance")
        public interface AddRootCompDocInstance {}

        @Request(
            uri = "AddRootCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddRootCompDocTemplate")
        public interface AddRootCompDocTemplate {}

        @Request(
            uri = "ViewInstances",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewInstances")
        public interface ViewInstances {}

        @Request(
            uri = "AddChildCompDocInstance",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddChildCompDocInstance")
        public interface AddChildCompDocInstance {}

        @Request(
            uri = "AddChildCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddChildCompDocTemplate")
        public interface AddChildCompDocTemplate {}

        @Request(
            uri = "ViewCompDocContentBinary",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocContentBinary")
        public interface ViewCompDocContentBinary {}

        @Request(
            uri = "createCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRootCompDoc")
        @Response(name = "error", type = "view", value = "EditRootCompDoc")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String createCompDocTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCompDoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String updateCompDoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "persistCompDocPdf2Survey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistCompDocPdf2Survey")
        public static String persistCompDocPdf2Survey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCompDoc",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String createCompDoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRootCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRootCompDoc")
        @Response(name = "error", type = "view", value = "EditRootCompDoc")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String updateRootCompDocTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createRootCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRootCompDoc")
        @Response(name = "error", type = "view", value = "AddRootCompDocTemplate")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String createRootCompDocTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createChildCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "AddChildCompDocTemplate")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String createChildCompDocTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createChildCompDocInstance",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "AddChildCompDocInstance")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String createChildCompDocInstance(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateChildCompDocTemplate",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String updateChildCompDocTemplate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadCompDocContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistCompDocContent")
        public static String uploadCompDocContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewCompDocContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocContent")
        public interface ViewCompDocContent {}

        @Request(
            uri = "updateChildCompDocInstance",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditChildCompDoc")
        @Response(name = "error", type = "view", value = "EditChildCompDoc")
        @Event(type = "service", invoke = "persistCompDoc")
        public static String updateChildCompDocInstance(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "genCompDocInstance",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRootCompDoc")
        @Response(name = "error", type = "view", value = "AddRootCompDocInstance")
        @Event(type = "service", invoke = "genCompDocInstance")
        public static String genCompDocInstance(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "resequenceCompDocPart",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocTemplateTree")
        @Response(name = "error", type = "view", value = "ViewCompDocTemplateTree")
        @Event(type = "service", invoke = "resequence")
        public static String resequenceCompDocPart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 20)
    public static class Part20 {
        @Request(
            uri = "GenCompDocPdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "error")
        public static String genCompDocPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.compdoc.CompDocEvents.genCompDocPdf
            return CompDocEvents.genCompDocPdf(request, response);
        }

        @Request(
            uri = "GenContentPdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "error")
        public static String genContentPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.compdoc.CompDocEvents.genContentPdf
            return CompDocEvents.genContentPdf(request, response);
        }

        @Request(
            uri = "EditCompDocContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCompDocContentRole")
        public interface EditCompDocContentRole {}

        @Request(
            uri = "addCompDocContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCompDocContentRole")
        @Response(name = "error", type = "view", value = "EditCompDocContentRole")
        @Event(type = "service", invoke = "createContentRole")
        public static String addCompDocContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCompDocContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCompDocContentRole")
        @Response(name = "error", type = "view", value = "EditCompDocContentRole")
        @Event(type = "service", invoke = "updateContentRole")
        public static String updateCompDocContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeCompDocContentRole",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCompDocContentRole")
        @Response(name = "error", type = "view", value = "EditCompDocContentRole")
        @Event(type = "service", invoke = "removeContentRole")
        public static String removeCompDocContentRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentApproval")
        public interface ListContentApproval {}

        @Request(
            uri = "ListWaitingContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWaitingContentApproval")
        public interface ListWaitingContentApproval {}

        @Request(
            uri = "EditContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentApproval")
        public interface EditContentApproval {}

        @Request(
            uri = "createContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentApproval")
        @Response(name = "error", type = "view", value = "ListContentApproval")
        @Event(type = "service", invoke = "createContentApproval")
        public static String createContentApproval(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentApproval")
        @Response(name = "error", type = "view", value = "ListContentApproval")
        @Event(type = "service", invoke = "updateContentApproval")
        public static String updateContentApproval(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentApprovalStatus",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentApproval")
        @Response(name = "error", type = "view", value = "ListContentApproval")
        @Event(type = "service", invoke = "updateContentApproval")
        public static String updateContentApprovalStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWaitingContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListWaitingContentApproval")
        @Response(name = "error", type = "view", value = "ListWaitingContentApproval")
        @Event(type = "service", invoke = "updateContentApproval")
        public static String updateWaitingContentApproval(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentApproval")
        @Response(name = "error", type = "view", value = "ListContentApproval")
        @Event(type = "service", invoke = "removeContentApproval")
        public static String removeContentApproval(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "prepForApproval",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCompDocInstanceTree")
        @Response(name = "error", type = "view", value = "ViewCompDocInstanceTree")
        @Event(type = "service", invoke = "prepForApproval")
        public static String prepForApproval(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListContentRevisions",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentRevisions")
        public interface ListContentRevisions {}

        @Request(
            uri = "EditContentRevision",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevision")
        public interface EditContentRevision {}

        @Request(
            uri = "createContentRevision",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevision")
        @Response(name = "error", type = "view", value = "EditContentRevision")
        @Event(type = "service", invoke = "createContentRevision")
        public static String createContentRevision(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentRevision",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevision")
        @Response(name = "error", type = "view", value = "EditContentRevision")
        @Event(type = "service", invoke = "updateContentRevision")
        public static String updateContentRevision(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentRevision",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevision")
        @Response(name = "error", type = "view", value = "EditContentRevision")
        @Event(type = "service", invoke = "removeContentRevision")
        public static String removeContentRevision(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 21)
    public static class Part21 {
        @Request(
            uri = "ListContentRevisionItem",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListContentRevisionItem")
        public interface ListContentRevisionItem {}

        @Request(
            uri = "EditContentRevisionItem",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevisionItem")
        public interface EditContentRevisionItem {}

        @Request(
            uri = "createContentRevisionItem",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevisionItem")
        @Response(name = "error", type = "view", value = "EditContentRevisionItem")
        @Event(type = "service", invoke = "createContentRevisionItem")
        public static String createContentRevisionItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentRevisionItem",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevisionItem")
        @Response(name = "error", type = "view", value = "EditContentRevisionItem")
        @Event(type = "service", invoke = "updateContentRevisionItem")
        public static String updateContentRevisionItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentRevisionItem",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentRevisionItem")
        @Response(name = "error", type = "view", value = "EditContentRevisionItem")
        @Event(type = "service", invoke = "removeContentRevisionItem")
        public static String removeContentRevisionItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSurvey")
        public interface FindSurvey {}

        @Request(
            uri = "ListFindSurveySearchResults",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFindSurveySearchResults")
        public interface ListFindSurveySearchResults {}

        @Request(
            uri = "EditSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurvey")
        public interface EditSurvey {}

        @Request(
            uri = "createSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurvey")
        @Response(name = "error", type = "view", value = "EditSurvey")
        @Event(type = "service", invoke = "createSurvey")
        public static String createSurvey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurvey")
        @Response(name = "error", type = "view", value = "EditSurvey")
        @Event(type = "service", invoke = "updateSurvey")
        public static String updateSurvey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "buildSurveyFromPdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurvey")
        @Response(name = "error", type = "view", value = "EditSurvey")
        @Event(type = "service", invoke = "buildSurveyFromPdf")
        public static String buildSurveyFromPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "buildSurveyResponseFromPdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyResponse")
        @Response(name = "error", type = "view", value = "EditSurveyResponse")
        @Event(type = "service", invoke = "buildSurveyResponseFromPdf")
        public static String buildSurveyResponseFromPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurvey")
        @Response(name = "error", type = "view", value = "EditSurvey")
        @Event(type = "service", invoke = "deleteSurvey")
        public static String deleteSurvey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSurveyMultiResps",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        public interface EditSurveyMultiResps {}

        @Request(
            uri = "createSurveyMultiResp",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "createSurveyMultiResp")
        public static String createSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyMultiResp",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "updateSurveyMultiResp")
        public static String updateSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSurveyMultiResp",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "deleteSurveyMultiResp")
        public static String deleteSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSurveyMultiRespColumn",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "createSurveyMultiRespColumn")
        public static String createSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyMultiRespColumn",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "updateSurveyMultiRespColumn")
        public static String updateSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSurveyMultiRespColumn",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyMultiResps")
        @Response(name = "error", type = "view", value = "EditSurveyMultiResps")
        @Event(type = "service", invoke = "deleteSurveyMultiRespColumn")
        public static String deleteSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 22)
    public static class Part22 {
        @Request(
            uri = "EditSurveyQuestions",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        public interface EditSurveyQuestions {}

        @Request(
            uri = "createSurveyQuestion",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "createSurveyQuestion")
        public static String createSurveyQuestion(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyQuestion",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "updateSurveyQuestion")
        public static String updateSurveyQuestion(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSurveyQuestion",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "deleteSurveyQuestion")
        public static String deleteSurveyQuestion(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSurveyQuestionOption",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "createSurveyQuestionOption")
        public static String createSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyQuestionOption",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "updateSurveyQuestionOption")
        public static String updateSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSurveyQuestionOption",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "deleteSurveyQuestionOption")
        public static String deleteSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSurveyQuestionCategory",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "createSurveyQuestionCategory")
        public static String createSurveyQuestionCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSurveyQuestionAppl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "createSurveyQuestionAppl")
        public static String createSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyQuestionAppl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "updateSurveyQuestionAppl")
        public static String updateSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSurveyQuestionAppl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "deleteSurveyQuestionAppl")
        public static String removeSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSurveyPage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "createSurveyPage")
        public static String createSurveyPage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSurveyPage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "updateSurveyPage")
        public static String updateSurveyPage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSurveyPage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyQuestions")
        @Response(name = "error", type = "view", value = "EditSurveyQuestions")
        @Event(type = "service", invoke = "deleteSurveyPage")
        public static String removeSurveyPage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindSurveyResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSurveyResponse")
        public interface FindSurveyResponse {}

        @Request(
            uri = "ViewSurveyResponses",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSurveyResponses")
        public interface ViewSurveyResponses {}

        @Request(
            uri = "EditSurveyResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSurveyResponse")
        public interface EditSurveyResponse {}

        @Request(
            uri = "updateSurveyResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSurveyResponses")
        @Response(name = "error", type = "view", value = "EditSurveyResponse")
        @Event(type = "service", invoke = "createSurveyResponse")
        public static String updateSurveyResponse(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListLayout")
        public interface ListLayout {}

        @Request(
            uri = "clipFindLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindLayout")
        @Response(name = "error", type = "view", value = "FindLayout")
        public static String clipFindLayout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.copyToClip
            return LayoutEvents.copyToClip(request, response);
        }

    }

    // Auto-generated split (Part 23)
    public static class Part23 {
        @Request(
            uri = "FindLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindLayout")
        public interface FindLayout {}

        @Request(
            uri = "EditLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        public interface EditLayout {}

        @Request(
            uri = "AddLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddLayout")
        public interface AddLayout {}

        @Request(
            uri = "createLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        @Response(name = "error", type = "view", value = "AddLayout")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "createLayout")
        public static String createLayout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        @Response(name = "error", type = "view", value = "EditLayout")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "updateLayout")
        public static String updateLayout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createLayoutSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutSubContent")
        @Response(name = "error", type = "view", value = "EditLayoutSubContent")
        public static String createLayoutSubContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.createLayoutSubContent
            return LayoutEvents.createLayoutSubContent(request, response);
        }

        @Request(
            uri = "updateLayoutSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutSubContent")
        @Response(name = "error", type = "view", value = "EditLayoutSubContent")
        public static String updateLayoutSubContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.updateLayoutSubContent
            return LayoutEvents.updateLayoutSubContent(request, response);
        }

        @Request(
            uri = "replaceSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        @Response(name = "error", type = "view", value = "EditLayout")
        public static String replaceSubContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.replaceSubContent
            return LayoutEvents.replaceSubContent(request, response);
        }

        @Request(
            uri = "pasteSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        @Response(name = "error", type = "view", value = "EditLayout")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "pasteSubContent")
        public static String pasteSubContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListLayout")
        @Response(name = "error", type = "view", value = "main")
        @Event(type = "service", invoke = "removeContentAssoc")
        public static String removeLayout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditLayoutHtml",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutHtml")
        public interface EditLayoutHtml {}

        @Request(
            uri = "EditLayoutText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutText")
        public interface EditLayoutText {}

        @Request(
            uri = "EditLayoutImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutImage")
        public interface EditLayoutImage {}

        @Request(
            uri = "EditLayoutUrl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutUrl")
        public interface EditLayoutUrl {}

        @Request(
            uri = "createLayoutText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutText")
        @Response(name = "error", type = "view", value = "EditLayoutText")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "createLayoutText")
        public static String createLayoutText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditLayoutSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutSubContent")
        public interface EditLayoutSubContent {}

        @Request(
            uri = "updateLayoutText",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutText")
        @Response(name = "error", type = "view", value = "EditLayoutText")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "updateLayoutText")
        public static String updateLayoutText(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createLayoutHtml",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutHtml")
        @Response(name = "error", type = "view", value = "EditLayoutHtml")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "createLayoutText")
        public static String createLayoutHtml(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateLayoutHtml",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutHtml")
        @Response(name = "error", type = "view", value = "EditLayoutHtml")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "updateLayoutText")
        public static String updateLayoutHtml(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createLayoutUrl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutUrl")
        @Response(name = "error", type = "view", value = "EditLayoutUrl")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "createLayoutUrl")
        public static String createLayoutUrl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 24)
    public static class Part24 {
        @Request(
            uri = "updateLayoutUrl",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutUrl")
        @Response(name = "error", type = "view", value = "EditLayoutUrl")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/layout/LayoutEvents.xml", invoke = "updateLayoutUrl")
        public static String updateLayoutUrl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createLayoutImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutImage")
        @Response(name = "error", type = "view", value = "EditLayoutImage")
        public static String createLayoutImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.createLayoutImage
            return LayoutEvents.createLayoutImage(request, response);
        }

        @Request(
            uri = "updateLayoutImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutImage")
        @Response(name = "error", type = "view", value = "EditLayoutImage")
        public static String updateLayoutImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.updateLayoutImage
            return LayoutEvents.updateLayoutImage(request, response);
        }

        @Request(
            uri = "updateLayoutImageOnly",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayoutImage")
        @Response(name = "error", type = "view", value = "EditLayoutImage")
        @Event(type = "java", path = "org.ofbiz.content.layout.LayoutEvents", invoke = "updateLayoutImageOnly")
        public static String updateLayoutImageOnly(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cloneLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditLayout")
        @Response(name = "error", type = "view", value = "EditLayout")
        public static String cloneLayout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.layout.LayoutEvents.cloneLayout
            return LayoutEvents.cloneLayout(request, response);
        }

        @Request(
            uri = "UserPermissions",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UserPermissions")
        public interface UserPermissions {}

        @Request(
            uri = "CMSContentFind",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentFind")
        public interface CMSContentFind {}

        @Request(
            uri = "CMSContentEdit",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentEdit")
        public interface CMSContentEdit {}

        @Request(
            uri = "CMSSites",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSSites")
        public interface CMSSites {}

        @Request(
            uri = "updateSiteRoles",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSSites")
        @Response(name = "error", type = "view", value = "CMSSites")
        @Event(type = "service-multi", invoke = "updateSiteRoles")
        public static String updateSiteRoles(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "linkContentToPubPt",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentEdit")
        @Response(name = "error", type = "view", value = "CMSContentEdit")
        @Event(type = "service-multi", invoke = "linkContentToPubPt")
        public static String linkContentToPubPt(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFeatures",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentEdit")
        @Response(name = "error", type = "view", value = "CMSContentEdit")
        @Event(type = "service-multi", invoke = "updateOrRemove")
        public static String updateFeatures(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "publishResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSSites")
        @Response(name = "error", type = "view", value = "CMSSites")
        @Event(type = "service-multi", invoke = "updateContent")
        public static String publishResponse(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addSubSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "addSubSite")
        public interface AddSubSite {}

        @Request(
            uri = "postNewSubSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSSites")
        @Response(name = "error", type = "view", value = "CMSSites")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String postNewSubSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSSites")
        @Response(name = "error", type = "view", value = "CMSSites")
        @Event(type = "service", invoke = "deactivateAssocs")
        public static String removeSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAddContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAddContent")
        public interface EditAddContent {}

        @Request(
            uri = "persistContentStuff",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAddContent")
        @Response(name = "error", type = "view", value = "EditAddContent")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistContentStuff(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAddSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAddSubContent")
        public interface EditAddSubContent {}

        @Request(
            uri = "persistSubContentStuff",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAddContent")
        @Response(name = "error", type = "view", value = "EditAddSubContent")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistSubContentStuff(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 25)
    public static class Part25 {
        @Request(
            uri = "persistContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentEdit")
        @Response(name = "error", type = "view", value = "EditAddContent")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String persistContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "persistImage",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CMSContentEdit")
        @Response(name = "error", type = "view", value = "EditAddContent")
        public static String persistImage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.content.UploadContentAndImage.uploadContentAndImage
            return UploadContentAndImage.uploadContentAndImage(request, response);
        }

        @Request(
            uri = "ViewSimpleContent",
            controller = "content",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSimpleContent")
        public interface ViewSimpleContent {}

        @Request(
            uri = "LookupContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}

        @Request(
            uri = "LookupSubContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSubContent")
        public interface LookupSubContent {}

        @Request(
            uri = "LookupDataResource",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupDataResource")
        public interface LookupDataResource {}

        @Request(
            uri = "LookupListLayout",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupListLayout")
        public interface LookupListLayout {}

        @Request(
            uri = "LookupSurvey",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSurvey")
        public interface LookupSurvey {}

        @Request(
            uri = "LookupSurveyResponse",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSurveyResponse")
        public interface LookupSurveyResponse {}

        @Request(
            uri = "LookupTreeContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTreeContent")
        public interface LookupTreeContent {}

        @Request(
            uri = "LookupDetailContentTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupDetailContentTree")
        public interface LookupDetailContentTree {}

        @Request(
            uri = "LookupUserLoginAndPartyDetails",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUserLoginAndPartyDetails")
        public interface LookupUserLoginAndPartyDetails {}

        @Request(
            uri = "LookupPerson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupPartyAndUserLoginAndPerson",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyAndUserLoginAndPerson")
        public interface LookupPartyAndUserLoginAndPerson {}

        @Request(
            uri = "LookupProductFeature",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "LookupPartyName",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupWorkEffort",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "navigateContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "navigateContent")
        public interface NavigateContent {}

        @Request(
            uri = "updateDocumentTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "navigateContent")
        @Response(name = "error", type = "view", value = "navigateContent")
        @Event(type = "service", invoke = "updateContent")
        public static String updateDocumentTree(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeDocumentFromTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "navigateContent")
        @Response(name = "error", type = "view", value = "navigateContent")
        @Event(type = "service", invoke = "removeContentAssoc")
        public static String removeDocumentFromTree(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 26)
    public static class Part26 {
        @Request(
            uri = "addDocumentToTree",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "navigateContent")
        @Response(name = "error", type = "request", value = "navigateContent")
        @Event(type = "simple", path = "component://content/script/org/ofbiz/content/content/ContentEvents.xml", invoke = "createDocument")
        public static String addDocumentToTree(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "showContent",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showContent")
        public interface ShowContent {}

        @Request(
            uri = "showContentPdf",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showContentPdf")
        public interface ShowContentPdf {}

        @Request(
            uri = "ContentSearchOptions",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ContentSearchOptions")
        public interface ContentSearchOptions {}

        @Request(
            uri = "ContentSearchResults",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ContentSearchResults")
        public interface ContentSearchResults {}

        @Request(
            uri = "EditContentKeywords",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentKeywords")
        public interface EditContentKeywords {}

        @Request(
            uri = "createContentKeyword",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentKeywords")
        @Response(name = "error", type = "view", value = "EditContentKeywords")
        @Event(type = "service", invoke = "createContentKeyword")
        public static String createContentKeyword(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteContentKeyword",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentKeywords")
        @Response(name = "error", type = "view", value = "EditContentKeywords")
        @Event(type = "service", invoke = "deleteContentKeyword")
        public static String deleteContentKeyword(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteContentKeywords",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditContentKeywords")
        @Response(name = "error", type = "view", value = "EditContentKeywords")
        @Event(type = "service", invoke = "deleteContentKeywords")
        public static String deleteContentKeywords(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentAllKeywords",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindContent")
        @Response(name = "error", type = "view", value = "FindContent")
        public static String updateContentAllKeywords(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.content.ContentEvents.updateAllContentKeywords
            return ContentEvents.updateAllContentKeywords(request, response);
        }

        @Request(
            uri = "WebSiteSeo",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteSEO")
        public interface WebSiteSeo {}

        @Request(
            uri = "generateMissingSeoUrlForWebsite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteSEO")
        @Response(name = "error", type = "view", value = "WebSiteSEO")
        @Event(type = "service", invoke = "generateWebsiteAlternativeUrls")
        public static String generateMissingSeoUrlForWebsite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSeoUrlForWebsite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteSEO")
        @Response(name = "error", type = "view", value = "WebSiteSEO")
        @Event(type = "service", invoke = "removeWebsiteAlternativeUrls")
        public static String removeSeoUrlForWebsite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "generateSitemapFilesForWebsite",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteSEO")
        @Response(name = "error", type = "view", value = "WebSiteSEO")
        @Event(type = "service", invoke = "generateWebsiteAlternativeUrlSitemapFiles")
        public static String generateSitemapFilesForWebsite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "WebSiteContactList",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        public interface WebSiteContactList {}

        @Request(
            uri = "createWebSiteContactList",
            controller = "content",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "createWebSiteContactList")
        public static String createWebSiteContactList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWebSiteContactList",
            controller = "content",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "updateWebSiteContactList")
        public static String updateWebSiteContactList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWebSiteContactList",
            controller = "content",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "WebSiteContactList")
        @Response(name = "error", type = "view", value = "WebSiteContactList")
        @Event(type = "service", invoke = "deleteWebSiteContactList")
        public static String deleteWebSiteContactList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "WebAnalytics",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWebAnalyticsConfigs")
        @Response(name = "error", type = "view", value = "FindWebAnalyticsConfigs")
        public interface WebAnalytics {}

        @Request(
            uri = "WebAnalyticsConfigs",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWebAnalyticsConfigs")
        @Response(name = "error", type = "view", value = "FindWebAnalyticsConfigs")
        public interface WebAnalyticsConfigs {}

    }

    // Auto-generated split (Part 27)
    public static class Part27 {
        @Request(
            uri = "FindWebAnalyticsConfigs",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindWebAnalyticsConfigs")
        @Response(name = "error", type = "view", value = "FindWebAnalyticsConfigs")
        public interface FindWebAnalyticsConfigs {}

        @Request(
            uri = "EditWebAnalyticsConfig",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebAnalyticsConfig")
        @Response(name = "error", type = "view", value = "EditWebAnalyticsConfig")
        public interface EditWebAnalyticsConfig {}

        @Request(
            uri = "createWebAnalyticsConfig",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "FindWebAnalyticsConfigs")
        @Response(name = "error", type = "view", value = "EditWebAnalyticsConfig")
        @Event(type = "service", invoke = "createWebAnalyticsConfig")
        public static String createWebAnalyticsConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWebAnalyticsConfig",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "FindWebAnalyticsConfigs")
        @Response(name = "error", type = "view", value = "EditWebAnalyticsConfig")
        @Event(type = "service", invoke = "updateWebAnalyticsConfig")
        public static String updateWebAnalyticsConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteWebAnalyticsConfig",
            controller = "content",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "FindWebAnalyticsConfigs")
        @Event(type = "service", invoke = "deleteWebAnalyticsConfig")
        public static String deleteWebAnalyticsConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }


    }
}
