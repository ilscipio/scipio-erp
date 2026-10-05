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
public class ContentsetupContentSetupScreens {

    @Screen(name = "EditContentType", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentType")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "type")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentType {}

    @Screen(name = "EditContentTypeAttr", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentAttribute")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "typeAttr")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentTypeAttr", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentTypeAttrPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentTypeAttr", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentTypeAttr {}

    @Screen(name = "EditContentAssocType", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentAssoc")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "assocType")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentAssocType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentAssocTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentAssocType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentAssocType {}

    @Screen(name = "EditContentPurposeType", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentPurpose")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "purposeType")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentPurposeType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentPurposeTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentPurposeType", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentPurposeType {}

    @Screen(name = "EditContentAssocPredicate", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentAssocPredicate")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "assocPred")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentAssocPredicate", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentAssocPredicatePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentAssocPredicate", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentAssocPredicate {}

    @Screen(name = "EditContentPurposeOperation", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentPurposeOperation")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "contentPurposeOp")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentPurposeOperation", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentPurposeOperationPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentPurposeOperation", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentPurposeOperation {}

    @Screen(name = "UserPermissions", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentUserPermissions")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "userpermission")
    @Action(type = ActionType.SET, field = "webSitePublishPoint", fromField = "parameters.webSitePublishPoint", defaultValue = "OFBIZDOCROOT")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/contentsetup/UserPermPrep.groovy")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/contentsetup/UserPermissions.ftl"
            )})
        }
    )
    public interface UserPermissions {}

    @Screen(name = "EditContentOperation", location = "component://content/widget/contentsetup/ContentSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentOperation")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "contentOp")
    @DecoratorScreen(
        name = "CommonContentSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateContentOperation", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "ContentOperationPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentOperation", location = "component://content/widget/contentsetup/ContentSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentOperation {}

}
