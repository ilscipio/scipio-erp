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
public class ContactListScreens {

    @Screen(name = "FindContactLists", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingContactListFindContactLists")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindContactLists")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindContactList")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactList"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindContactLists", location = "component://marketing/widget/ContactListForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContactLists", location = "component://marketing/widget/ContactListForms.xml"
                        )}))})})
        }
    )
    public interface FindContactLists {}

    @Screen(name = "EditContactList", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContactList")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditContactList")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListContactLists")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"contactList"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditContactList", location = "component://marketing/widget/ContactListForms.xml", position = 1
                        )}, containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactList"
                            )}, position = 0)})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleAddContactList}", includeForms = {
                                    @IncludeForm(name = "EditContactList", location = "component://marketing/widget/ContactListForms.xml"
                                )})}))})
        }
    )
    public interface EditContactList {}

    @Screen(name = "ListContactLists", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListContactList")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListContactList")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListContactList")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "entityName", value = "ContactList")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListContactLists", location = "component://marketing/widget/ContactListForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactList"
                    )}, position = 0)})})
        }
    )
    public interface ListContactLists {}

    @Screen(name = "EditContactListParty", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContactListParty")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListParty")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditContactListParty")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListParties?contactListId=${contactListId}")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactListParty", valueField = "contactListParty")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContactListPartyAndStatus", list = "contactListPartyStatusList", conditions = {@ConditionExpr(fieldName = "contactListId", fromField = "contactListId"), @ConditionExpr(fieldName = "partyId", fromField = "partyId"), @ConditionExpr(fieldName = "fromDate", fromField = "fromDate")}, orderBy = {"-statusDate"})
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContactListParty", location = "component://marketing/widget/ContactListForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.CommonStatusHistory}", style = "heading", position = 2
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListPartyCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListParty"
                    )}, position = 0)}, widgets = {
                        @Widget(type = WidgetType.ITERATE_SECTION, list = "contactListPartyStatusList", entry = "contactListPartyStatus", name = "EditContactListParty-iterate1", location = "component://marketing/widget/ContactListScreens.xml", position = 3
                    )})})
        }
    )
    public interface EditContactListParty {}

    @Screen(name = "EditContactListParty-iterate1", location = "component://marketing/widget/ContactListScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${contactListPartyStatus.statusDate} ${contactListPartyStatus.description} [by: ${contactListPartyStatus.setByUserLoginId}] [code: ${contactListPartyStatus.optInVerifyCode}]")})}))
    public interface EditContactListParty_iterate1 {}

    @Screen(name = "ListContactListParties", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListContactListParty")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListParty")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListContactListParty")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListParties?contactListId=${contactListId}")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListContactListParties", location = "component://marketing/widget/ContactListForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListPartyCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListParty"
                    )}, position = 0)})})
        }
    )
    public interface ListContactListParties {}

    @Screen(name = "FindContactListParties", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindContactListParty")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListParty")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindContactListParty")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListParties?contactListId=${contactListId}")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListPartyCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListParty"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindContactListParties", location = "component://marketing/widget/ContactListForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContactListParties", location = "component://marketing/widget/ContactListForms.xml"
                        )}))})})
        }
    )
    public interface FindContactListParties {}

    @Screen(name = "EditContactListCommEvent", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContactListCommEvent")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditContactListCommEvent")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListCommEvents?contactListId=${parameters.contactListId}")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_AND, entityName = "CommunicationEventType", list = "communicationEventTypes", fieldMaps = {@FieldMap(fieldName = "contactMechTypeId", fromField = "contactList.contactMechTypeId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "status")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/contact/GetContactListMarketingEmail.groovy")
    @Action(type = ActionType.SET, field = "contactMechIdFrom", value = "${marketingEmail.contactMechId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${contactList.ownerPartyId}")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContactListCommEvent", location = "component://marketing/widget/ContactListForms.xml", position = 2
                )}, includeMenus = {
                    @IncludeMenu(name = "ContactListCommBar", location = "component://marketing/widget/ContactListMenus.xml", position = 0
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCommEventCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListCommEvent"
                    )}, position = 1)})})
        }
    )
    public interface EditContactListCommEvent {}

    @Screen(name = "ListContactListCommEvents", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListContactListCommEvent")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListContactListCommEvent")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListCommEvents?contactListId=${parameters.contactListId}")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListContactList} ${uiLabelMap.CommonFor} contactListId=${contactListId}", includeForms = {
                    @IncludeForm(name = "ListContactListCommEvents", location = "component://marketing/widget/ContactListForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCommEventCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListCommEvent"
                    )}, position = 0)})})
        }
    )
    public interface ListContactListCommEvents {}

    @Screen(name = "FindContactListCommEvents", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindContactListCommEvents")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindContactListCommEvents")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindContactListCommEvents?contactListId=${parameters.contactListId}")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "FindContactListCommEvents", location = "component://marketing/widget/ContactListForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingContactListCommEventCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactListCommEvent"
                    )}, position = 0)})})
        }
    )
    public interface FindContactListCommEvents {}

    @Screen(name = "FindImportContactListParties", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindImportContactListParties")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListImportParty")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindImportContactListParties")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "FindImportContactListParties?contactListId=${parameters.contactListId}")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.SET, field = "contactMechTypeId", fromField = "contactList.contactMechTypeId")
    @Action(type = ActionType.SET, field = "selectedFields[+0]", value = "partyId")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindImportContactListParties", location = "component://marketing/widget/ContactListForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListImportContactListParties", location = "component://marketing/widget/ContactListForms.xml"
                    )}))})})
        }
    )
    public interface FindImportContactListParties {}

    @Screen(name = "LookupContactList", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLookupContactList")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleLookupContactList")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupContactList}")
    @Action(type = ActionType.SET, field = "entityName", value = "ContactList")
    @Action(type = ActionType.SET, field = "searchFields", value = "[contactListId, contactListName, description]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupContactList", location = "component://marketing/widget/ContactListForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupContactList", location = "component://marketing/widget/ContactListForms.xml"
            )})
        }
    )
    public interface LookupContactList {}

    @Screen(name = "LookupPreferredContactMech", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePreferredContactMech")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitlePreferredContactMech")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitlePreferredContactMech}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListContactLists")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.parm0")
    @Action(type = ActionType.SET, field = "entityName", value = "PartyAndContactMech")
    @Action(type = ActionType.SET, field = "searchFields", value = "[contactMechId, partyId, infoString, paToName, paAddress1]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPreferredContactMech", location = "component://marketing/widget/ContactListForms.xml"
            )})
        }
    )
    public interface LookupPreferredContactMech {}

    @Screen(name = "PreviewContactListCommEvent", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContactListCommEvent")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "ContactListCommEvent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditContactListCommEvent")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactMech", valueField = "contactMech", fieldMaps = {@FieldMap(fieldName = "contactMechId", fromField = "communicationEvent.contactMechIdFrom")})
    @Action(type = ActionType.SET, field = "content", value = "${groovy:org.ofbiz.base.util.StringUtil.wrapString(communicationEvent.content)}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/marketing/contact/ContactCommunicationPreview.ftl")}))
    public interface PreviewContactListCommEvent {}

    @Screen(name = "DefaultOptOutScreen", location = "component://marketing/widget/ContactListScreens.xml")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "Opt-Out Results", containers = {
                    @Container(labels = {
                        @Label(text = "You have been successfully removed from the ${contactList.contactListName} mailing list!"
                    )})})})
        }
    )
    public interface DefaultOptOutScreen {}

    @Screen(name = "OptOutResponse", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "optOutOfListFromCommEvent", resultMapName = "optOutResult")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList", fieldMaps = {@FieldMap(fieldName = "contactListId", fromField = "optOutResult.contactListId")})
    @Action(type = ActionType.SET, field = "contactListId", fromField = "contactList.contactListId")
    @Action(type = ActionType.SET, field = "screenName", fromField = "contactList.optOutScreen", defaultValue = "component://marketing/widget/ContactListScreens.xml#DefaultOptOutScreen")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${screenName}", shareScope = true)}))
    public interface OptOutResponse {}

    @Screen(name = "WebSiteContactList", location = "component://marketing/widget/ContactListScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingWebSiteContactList")
    @Action(type = ActionType.SET, field = "activeContactListSubMenuItem", value = "WebSiteContactList")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.ENTITY_AND, entityName = "WebSiteContactList", list = "webSiteContactLists", fieldMaps = {@FieldMap(fieldName = "contactListId", fromField = "contactList.contactListId")}, orderBy = {"-fromDate"})
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingWebSiteContactListCreate}", includeForms = {
                    @IncludeForm(name = "CreateWebSiteContactList", location = "component://marketing/widget/ContactListForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.MarketingWebSiteContactListView} of contactListId[${parameters.contactListId}]", includeForms = {
                    @IncludeForm(name = "ViewWebSiteContactList", location = "component://marketing/widget/ContactListForms.xml"
                )})})
        }
    )
    public interface WebSiteContactList {}

}
