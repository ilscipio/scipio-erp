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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaContactScreens {

    @Screen(name = "FindContacts", location = "component://marketing/widget/sfa/ContactScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "currentUrl", value = "FindContacts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Contacts")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/FindContacts_script1.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.SfaContacts}")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonContactDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ContactSubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
            )}, containers = {
                @Container(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindContacts", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(widgets = @WidgetsLeaf(includeForms = {
                            @IncludeForm(name = "ListContacts", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                        )}))}))}))})
        }
    )
    public interface FindContacts {}

    @Screen(name = "NewContact", location = "component://marketing/widget/sfa/ContactScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateContact")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Contacts")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @DecoratorScreen(
        name = "CommonContactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewContact", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                )})})
        }
    )
    public interface NewContact {}

    @Screen(name = "MergeContacts", location = "component://marketing/widget/sfa/ContactScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateContact")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/sfa/SfaMenus.xml#Contact")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MergeContacts")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/sfa/WEB-INF/action/MergeContacts.groovy")
    @DecoratorScreen(
        name = "CommonContactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.SfaMergeContacts}", includeForms = {
                    @IncludeForm(name = "MergeContacts", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = CompareField.class, params = {"parameters.partyIdFrom", "not-equals", "parameters.partyIdTo"
                    })}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"contactInfoList"})}
                        ), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.SfaMergeContacts}", htmlTemplates = {
                    @HtmlTemplate(location = "component://marketing/webapp/sfa/contact/mergeContacts.ftl"
                
                        )})}))}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SfaCanNotMergeSameContact}"
                        )}))})
        }
    )
    public interface MergeContacts {}

    @Screen(name = "NewContactFromVCard", location = "component://marketing/widget/sfa/ContactScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateContactFromVCard")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Contacts")
    @DecoratorScreen(
        name = "CommonContactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonWarning}: ${uiLabelMap.CommonUnsupportedFunction}", style = "common-msg-warning"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewContactFromVCard", location = "component://marketing/widget/sfa/forms/ContactForms.xml", position = 1
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.SfaAutoCreateContactByImportingVCard}"
                    )}, position = 0)})})
        }
    )
    public interface NewContactFromVCard {}

    @Screen(name = "ViewPartiesCreatedByVCard", location = "component://marketing/widget/sfa/ContactScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateContactFromVCard")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Contacts")
    @DecoratorScreen(
        name = "CommonContactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonWarning}: ${uiLabelMap.CommonUnsupportedFunction}", style = "common-msg-warning"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifNotEmpty = {"parameters.partiesCreated", "parameters.partiesExist"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "partiesCreated", fromField = "parameters.partiesCreated"
                ),
                @Action(type = ActionType.SET, field = "partiesExist", fromField = "parameters.partiesExist"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingPartiesLoaded}", includeForms = {
                    @IncludeForm(name = "ViewPartiesCreatedByVCard", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.MarketingPartiesNotLoadedAlreadyExist}", includeForms = {
                    @IncludeForm(name = "ViewPartiesExistInVCard", location = "component://marketing/widget/sfa/forms/ContactForms.xml"
                )})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.MarketingNoPartyLoad}", style = "common-msg-result-norecord"
                )}))})
        }
    )
    public interface ViewPartiesCreatedByVCard {}

}
