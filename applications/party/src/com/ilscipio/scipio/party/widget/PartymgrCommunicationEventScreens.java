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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrCommunicationEventScreens {

    @Screen(name = "PendingCommunications", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePendingCommunications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "pending")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyIdFrom}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyIdTo}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEvent")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyPendingCommunicationEvents}", style = "heading", position = 2
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPendingCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 4
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyNewCommunication}", style = "${styles.link_nav} ${styles.action_add}", target = "ViewCommunicationEvent"
                )}, position = 3)}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"partyId"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "ProfileTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
                        )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface PendingCommunications {}

    @Screen(name = "ListPartyCommEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyCommunications}: ${parameters.partyId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "CommunicationEvent")
    @Action(type = ActionType.SERVICE, serviceName = "findPartyInSalesOpportunityRole", resultMapName = "leadPartyResult", fieldMaps = {@FieldMap(fieldName = "salesOpportunityId", fromField = "parameters.salesOpportunityId"), @FieldMap(fieldName = "roleTypeId", value = "LEAD")})
    @Action(type = ActionType.SET, field = "partyId", fromField = "leadPartyResult.partyId", defaultValue = "${parameters.partyId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "Party", list = "partyperson", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "partyTypeId", value = "PERSON")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEvents", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", value = "${partyId}")}, orderBy = {"-entryDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "contacts", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyIdFrom", fromField = "partyId"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"), @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")}, orderBy = {"partyIdTo"})
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"contacts"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HORIZONTAL_SEPARATOR),
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleListCommunicationsRelatedParties} ${partyId}", style = "heading"
                        ),
                        @Widget(type = WidgetType.ITERATE_SECTION, list = "contacts", entry = "contact", name = "ListPartyCommEvents-iterate1", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
                    )})),
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"partyperson"})}), actions = @Actions(value = {
                            @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRelationship", list = "accounts", conditions = {
                                @ConditionExpr(fieldName = "partyIdTo", fromField = "partyId"
                            ),
                            @ConditionExpr(fieldName = "roleTypeIdFrom", value = "ACCOUNT"
                        ),
                        @ConditionExpr(fieldName = "roleTypeIdTo", value = "CONTACT")
                    }, selectFields = {"partyIdFrom"}),
                    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "accounts[0].partyIdFrom"
                ),
                @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEvents", conditions = {
                    @ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "partyIdFrom"
                )}, orderBy = {"-entryDate"}),
                @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "contacts", filterByDate = true, fieldMaps = {
                    @FieldMap(fieldName = "partyIdFrom", fromField = "partyIdFrom"
                ),
                @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT"),
                @FieldMap(fieldName = "roleTypeIdTo", value = "CONTACT")}, orderBy = {"partyIdTo"
            })}), widgets = @WidgetsForContainer(sections = {
                @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifNotEmpty = {"partyIdFrom", "contacts"})}), widgets = @WidgetsForContainer2(value = {
                        @Widget(type = WidgetType.HORIZONTAL_SEPARATOR),
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleListCommunicationsRelatedParties} ${partyId}", style = "heading"
                    ),
                    @Widget(type = WidgetType.LABEL, text = "${partyIdFrom}", style = "heading+1"
                ),
                @Widget(type = WidgetType.ITERATE_SECTION, list = "accounts", entry = "account", name = "ListPartyCommEvents-iterate2", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
            ),
            @Widget(type = WidgetType.ITERATE_SECTION, list = "contacts", entry = "contact", name = "ListPartyCommEvents-iterate3", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
            )}))}))}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
            )}))})
        }
    )
    public interface ListPartyCommEvents {}

    @Screen(name = "ListPartyCommEvents-iterate1", location = "component://party/widget/partymgr/CommunicationEventScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"contacts"})}))
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEvents", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", value = "${contact.partyIdTo}")}, orderBy = {"-entryDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${contact.partyIdTo}", style = "heading+1"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}))
    public interface ListPartyCommEvents_iterate1 {}

    @Screen(name = "ListPartyCommEvents-iterate2", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}))
    public interface ListPartyCommEvents_iterate2 {}

    @Screen(name = "ListPartyCommEvents-iterate3", location = "component://party/widget/partymgr/CommunicationEventScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"contacts"})}))
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEvents", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", value = "${contact.partyIdTo}")}, orderBy = {"-entryDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"contact.partyIdTo", "not-equals", "${partyId}"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${contact.partyIdTo}", style = "heading+1"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}))
    public interface ListPartyCommEvents_iterate3 {}

    @Screen(name = "ListUnknownPartyComms", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListUnknownPartyComms")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListUnknownPartyComms")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "ListUnknownPartyEmails", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ListUnknownPartyComms {}

    @Screen(name = "FindCommunicationByOrder", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindCommunicationByOrder")
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEventAndOrder")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindCommunicationByOrder", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCommunicationByOrder", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )}))})), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindCommunicationByOrder {}

    @Screen(name = "FindCommunicationEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCommunicationEvents")
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEventAndRole")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )}))})), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindCommunicationEvents {}

    @Screen(name = "ViewCommunicationEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewCommunication")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "OverView")
    @Action(type = ActionType.SET, field = "parentCommEventId", fromField = "parameters.parentCommEventId")
    @Action(type = ActionType.SERVICE, serviceName = "setCommEventRoleToRead")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "commOverview", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
            )})
        }
    )
    public interface ViewCommunicationEvent {}

    @Screen(name = "commOverview", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(style = "${styles.grid_row}", containers = {@Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "commEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.FormFieldTitle_communicationEventId} ${parameters.communicationEventId}", position = 0)}), @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", labels = {@Label(text = "${uiLabelMap.CommonRelatedInformation}", style = "heading", position = 0)}, sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"communicationEvent.contactListId"})}), widgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCommEventRoles}", includeForms = {
                    @IncludeForm(name = "ViewCommRoles", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), failWidgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(title = "${uiLabelMap.MarketingContactListCommStatus}", includeForms = {
                    @IncludeForm(name = "ListContactListCommStatuses", location = "component://marketing/widget/ContactListForms.xml"
                )})}), position = 1), @SectionNested2(actions = @Actions(value = {@Action(type = ActionType.ENTITY_AND, entityName = "CommunicationEvent", list = "commEvents", fieldMaps = {@FieldMap(fieldName = "parentCommEventId", fromField = "parameters.communicationEventId")})}), widgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyChildCommunicationEvents}", includeForms = {
                    @IncludeForm(name = "ListCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), position = 3), @SectionNested2(actions = @Actions(value = {@Action(type = ActionType.SET, field = "entityName", value = "CustRequestAndCommEvent"), @Action(type = ActionType.SET, field = "requestParameters.communicationEventId", fromField = "parameters.communicationEventId")}), widgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequestList}", includeForms = {
                    @IncludeForm(name = "ListRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}), position = 4)}, screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCommContent}", includeForms = {
                    @IncludeForm(name = "listCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}, position = 2)})})}))
    public interface commOverview {}

    @Screen(name = "commEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${parent} ${uiLabelMap.PartyCommunicationEvent}", sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "equals", "EMAIL_COMMUNICATION"}), @ConditionNode(type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "equals", "AUTO_EMAIL_COMM"})}), @Condition(type = Empty.class, params = {"communicationEvent.contactListId"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ViewEmail", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 1)}, sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"communicationEvent.partyIdFrom"}, ifCompare = {@IfCompare(field = "communicationEvent.statusId", operator = "equals", value = "COM_UNKNOWN_PARTY")})}), widgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(containers = {
                    @ContainerInScreenlet(labels = {
                        @Label(text = "${uiLabelMap.PartyOriginEmailNotKnown}")}),
                        @ContainerInScreenlet(labels = {
                            @Label(text = "${uiLabelMap.PartyEmailMessage}:")})}, includeForms = {
                                @IncludeForm(name = "allocateMsgToPartyForm", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                            )})}), position = 0)}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ViewCommEvent", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}))})}))
    public interface commEvent {}

    @Screen(name = "EditCommunicationEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCommunication")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CommunicationEvent")
    @Action(type = ActionType.SET, field = "my", fromField = "parameters.my")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "communicationEvent.partyIdFrom", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "contactMechIdTo", fromField = "parameters.contactMechIdTo", defaultValue = "${communicationEvent.contactMechIdTo}")
    @DecoratorScreen(
        name = "Common${my}CommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "editCommEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
            )})
        }
    )
    public interface EditCommunicationEvent {}

    @Screen(name = "EditRequestFromCommEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCommunication")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "my", fromField = "parameters.my")
    @DecoratorScreen(
        name = "Common${my}CommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyEditCustomerRequest}", includeForms = {
                    @IncludeForm(name = "EditRequestFromCommEvent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface EditRequestFromCommEvent {}

    @Screen(name = "UpdateCommRoles", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewCommRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommRoles")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "parentCommEventId", fromField = "parameters.parentCommEventId")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party", useCache = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson", useCache = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommEventRoles}", includeForms = {
                    @IncludeForm(name = "ListCommRoles", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PartyCommEventRoles}", includeForms = {
                    @IncludeForm(name = "AddEventRole", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface UpdateCommRoles {}

    @Screen(name = "UpdateCommPurposes", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewCommPurposes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommPurposes")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "parentCommEventId", fromField = "parameters.parentCommEventId")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party", useCache = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson", useCache = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommEventPurposes}", includeForms = {
                    @IncludeForm(name = "AddEventPurpose", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                ),
                @IncludeForm(name = "ListCommPurposes", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
            )})})
        }
    )
    public interface UpdateCommPurposes {}

    @Screen(name = "ListCommWorkEfforts", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCommWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommWorkEfforts")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyIdFrom}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyIdTo}")
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEvent")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommWorkEfforts}", includeForms = {
                    @IncludeForm(name = "ListCommWorkEfforts", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyNewCommWorkEffort}", style = "${styles.link_nav} ${styles.action_add}", target = "AddCommEventWorkEffort"
                    )}, position = 0)})})
        }
    )
    public interface ListCommWorkEfforts {}

    @Screen(name = "AddCommEventWorkEffort", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCommWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommWorkEfforts")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommWorkEfforts}", includeForms = {
                    @IncludeForm(name = "AddCommEventWorkEffort", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface AddCommEventWorkEffort {}

    @Screen(name = "EditCommEventWorkEffort", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCommWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommWorkEfforts")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommWorkEfforts}", includeForms = {
                    @IncludeForm(name = "AddCommEventWorkEffort", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface EditCommEventWorkEffort {}

    @Screen(name = "ListCommContent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyCommContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CommContent")
    @Action(type = ActionType.SET, field = "parameters.partyId", value = "${parameters.partyId}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyIdFrom}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyIdTo}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "communicationEventId", value = "${parameters.communicationEventId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.PartyCommContent}", includeForms = {
                    @IncludeForm(name = "listCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                
                    )}),
                    @ScreenletNested(title = "${uiLabelMap.PartyAttachContent}", includeForms = {
                    @IncludeForm(name = "uploadContent1", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                
                )})})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface ListCommContent {}

    @Screen(name = "AddCommContent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyNewCommContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CommContent")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyIdFrom}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyIdTo}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "communicationEventId", value = "${parameters.communicationEventId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "addCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface AddCommContent {}

    @Screen(name = "EditCommContent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CommContent")
    @Action(type = ActionType.SET, field = "partyId", value = "${parameters.partyId}")
    @Action(type = ActionType.SET, field = "partyIdFrom", value = "${parameters.partyIdFrom}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", value = "${parameters.partyIdTo}", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "communicationEventId", value = "${parameters.communicationEventId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommEventContentDataResource", valueField = "commEventContentDataResource", fieldMaps = {@FieldMap(fieldName = "communicationEventId", fromField = "parameters.communicationEventId"), @FieldMap(fieldName = "contentId", fromField = "parameters.contentId"), @FieldMap(fieldName = "fromDate", fromField = "parameters.fromDate"), @FieldMap(fieldName = "drDataResourceId", fromField = "parameters.dataResourceId")})
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyCommunicationEventPermissionCheck", "UPDATE"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleEditCommContent}", style = "heading", position = 1
                ),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "editCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 2
            )}, sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"partyId"})}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "ProfileTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
                    )}), position = 0),
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Regexp.class, params = {"commEventContentDataResource.drMimeTypeId", "text.*"
                    })}), actions = @Actions(value = {
                        @Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "electronicText", fieldMaps = {
                            @FieldMap(fieldName = "dataResourceId", fromField = "commEventContentDataResource.drDataResourceId"
                        )})}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.PageTitleEditCommContent}", includeForms = {
                    @IncludeForm(name = "editCommTextContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                
                        )})}), position = 3),
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Regexp.class, params = {"commEventContentDataResource.drMimeTypeId", "text.*"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.PartyViewImage}", includeForms = {
                    @IncludeForm(name = "uploadCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 0
                
                        )}, labels = {
                    @Label(text = "${uiLabelMap.PartyViewImage}", style = "heading+1", position = 1
                
                    )}, widgets = {
                    @Widget(type = WidgetType.CONTENT, dataResourceId = "${commEventContentDataResource.drDataResourceId}", position = 2
                
                )})}), position = 4)}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface EditCommContent {}

    @Screen(name = "PartyCommunicationEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "mycomm")
    @Action(type = ActionType.SET, field = "my", value = "My", global = true)
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "MyCommunicationEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml"
            )})
        }
    )
    public interface PartyCommunicationEvents {}

    @Screen(name = "MyCommunicationEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "internalNotesOnly", fromField = "internalNotesOnly", defaultValue = "false")
    @Action(type = ActionType.SET, field = "partyId", fromField = "communicationPartyId", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyCommunicationsOfParty}: ${partyName.firstName} ${partyName.middleName} ${partyName.lastName} ${partyName.groupName} [${partyId}] ", name = "myComms", includeMenus = {@IncludeMenu(name = "communicationsMenu", location = "component://party/widget/partymgr/PartyMenus.xml")}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"parameters.form"}, ifCompare = {@IfCompare(field = "parameters.form", operator = "equals", value = "list")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "my", fromField = "parameters.my", defaultValue = "My")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "listMyCommEvents")})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.form", "equals", "view"})}), actions = @Actions(value = {@Action(type = ActionType.SERVICE, serviceName = "setCommEventRoleToRead"), @Action(type = ActionType.SET, field = "parentCommEventId", fromField = "parameters.parentCommEventId"), @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "MyCommSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "commOverview")})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.form", "equals", "new"}), @Condition(type = Empty.class, params = {"communicationEvent"})}), actions = @Actions(value = {@Action(type = ActionType.SERVICE, serviceName = "createCommunicationEvent"), @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent"), @Action(type = ActionType.SET, field = "my", fromField = "parameters.my"), @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "communicationEvent.partyIdFrom", defaultValue = "${userLogin.partyId}"), @Action(type = ActionType.SET, field = "contactMechIdTo", fromField = "parameters.contactMechIdTo", defaultValue = "${communicationEvent.contactMechIdTo}")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "editCommEvent")}), failWidgets = @WidgetsForContainer(sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = Compare.class, params = {"parameters.form", "equals", "edit"}), @ConditionNode(type = And.class), @ConditionNode(parent = 1, type = Compare.class, params = {"parameters.form", "equals", "new"}), @ConditionNode(parent = 1, not = true, type = Empty.class, params = {"communicationEvent"})})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "my", fromField = "parameters.my"), @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "communicationEvent.partyIdFrom", defaultValue = "${userLogin.partyId}")}), widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "editCommEvent")}))})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.form", "equals", "request"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "parentCommEventId", fromField = "parameters.parentCommEventId"), @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyId", defaultValue = "${userLogin.partyId}"), @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId"), @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest"), @Action(type = ActionType.SET, field = "statusId", fromField = "custRequest.statusId"), @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "currentStatus"), @Action(type = ActionType.SET, field = "projectMgrExists", value = "${groovy:org.ofbiz.base.component.ComponentConfig.componentExists(\"projectmgr\")}")}), widgets = @WidgetsForContainer(sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"projectMgrExists", "equals", "true"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "fromPartyId", fromField = "communicationEvent.partyIdFrom"), @Action(type = ActionType.SCRIPT, location = "component://projectmgr/webapp/projectmgr/WEB-INF/actions/getLastRequestAssignment.groovy")}), widgets = @WidgetsForContainer2(screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequest}", includeForms = {
                    @IncludeForm(name = "EditCustRequest", location = "component://projectmgr/widget/forms/CustRequestForms.xml"
                )})}), failWidgets = @WidgetsForContainer2(sections = {@SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.small", "equals", "Y"})}), widgets = @WidgetsForContainer3(screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequest}", includeForms = {
                    @IncludeForm(name = "EditSmallCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}), failWidgets = @WidgetsForContainer3(screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequest}", includeForms = {
                    @IncludeForm(name = "EditCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}))}))}))})}))
    public interface MyCommunicationEvents {}

    @Screen(name = "UpdateCommOrders", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyViewCommOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommOrders")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommEventOrders}", includeForms = {
                    @IncludeForm(name = "ListCommOrders", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PartyAddCommEventOrder}", includeForms = {
                    @IncludeForm(name = "AddCommOrder", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface UpdateCommOrders {}

    @Screen(name = "UpdateCommProducts", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyViewCommProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "UpdateCommProducts")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo", defaultValue = "${userLogin.partyId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonCommunicationEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyCommEventProducts}", includeForms = {
                    @IncludeForm(name = "ListCommProducts", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PartyAddCommEventProduct}", includeForms = {
                    @IncludeForm(name = "AddCommProduct", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})})
        }
    )
    public interface UpdateCommProducts {}

    @Screen(name = "listMyCommEvents", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEventsUnknown", conditions = {@ConditionExpr(fieldName = "statusId", operator = "equals", value = "COM_UNKNOWN_PARTY"), @ConditionExpr(fieldName = "roleStatusId", operator = "not-equals", value = "COM_ROLE_COMPLETED"), @ConditionExpr(fieldName = "partyId", operator = "equals", value = "${partyId}")}, orderBy = {"entryDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEventDraft", conditions = {@ConditionExpr(fieldName = "statusId", operator = "equals", value = "COM_PENDING"), @ConditionExpr(fieldName = "partyId", operator = "equals", value = "${partyId}"), @ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "ORIGINATOR")}, orderBy = {"entryDate"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyName", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CommunicationEventAndRole", list = "commEventProgress", conditions = {@ConditionExpr(fieldName = "statusId", operator = "equals", value = "COM_IN_PROGRESS"), @ConditionExpr(fieldName = "partyId", operator = "equals", value = "${partyId}"), @ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "ORIGINATOR")}, orderBy = {"entryDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"commEventsUnknown"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyEmailsFromUnknownOrigin}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ListMyUnknownPartyEmails", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}), position = 0), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"commEventDraft"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyDraftEmails}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDraftEmails", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}), position = 2), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"commEventProgress"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyInProgresstEmails}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProgressEmails", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}), position = 3)}))
    public interface listMyCommEvents {}

    @Screen(name = "editCommEvent", location = "component://party/widget/partymgr/CommunicationEventScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.communicationEventId", fromField = "communicationEvent.communicationEventId")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "equals", "EMAIL_COMMUNICATION"}), @ConditionNode(type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "equals", "AUTO_EMAIL_COMM"})}), @Condition(type = Compare.class, params = {"my", "equals", "My"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.CONTAINER, style = "clear", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"communicationEvent"})}), widgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCreateAddEmail} ${uiLabelMap.CommonFrom} ${parameters.partyIdFrom}", includeForms = {
                    @IncludeForm(name = "EditEmail", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), failWidgets = @WidgetsForContainer(containers = {@Container2(style = "${styles.grid_row}", containers = {@Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCommEventRoles}", includeForms = {
                    @IncludeForm(name = "ListCommRolesInline", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 0
                )}, sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "AddEventRole_submitAction", fromField = "AddEventRole_submitAction", defaultValue = "javascript:(document.AddEventRole.communicationEventId.value=document.EditEmail.communicationEventId.value),(document.AddEventRole.datetimeStarted.value=document.EditEmail.datetimeStarted.value),(document.AddEventRole.partyIdTo.value=document.EditEmail.partyIdTo.value),(document.AddEventRole.subject.value=document.EditEmail.subject.value),(document.AddEventRole.content.value=document.EditEmail.content.value),(document.AddEventRole.submit())"
                    )}), widgets = @WidgetsLeaf(includeForms = {
                        @IncludeForm(name = "AddEventRole", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )}), position = 1)})}), @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCommContent}", includeForms = {
                    @IncludeForm(name = "listCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 0
                )}, sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "showProgress", value = "false", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "progressSuccessAction", value = "redirect;;MyCommunicationEvents?partyIdFrom=${partyId}&statusId=COM_PENDING&form=new&my=My&communicationEventTypeId=EMAIL_COMMUNICATION&communicationEventId=${communicationEvent.communicationEventId}&portalPageId=${parameters.portalPageId}"
                ),
                @Action(type = ActionType.SET, field = "progressOptions", value = "{}"
                ),
                @Action(type = ActionType.SET, field = "uploadContent_submitAction", value = "javascript:(document.uploadContent.datetimeStarted.value=document.EditEmail.datetimeStarted.value),(document.uploadContent.partyIdTo.value=document.EditEmail.partyIdTo.value),(document.uploadContent.subject.value=document.EditEmail.subject.value),(document.uploadContent.content.value=document.EditEmail.content.value),(jQuery(document.uploadContent).trigger('submit')),void(0)"
                )}), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "uploadContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}), position = 1)})})})}, screenlets = {@ScreenletNested(title = "${uiLabelMap.CommonFrom}: ${communicationEvent.partyIdFrom}, CommunicationEventId: ${communicationEvent.communicationEventId}", includeForms = {
                    @IncludeForm(name = "EditEmail", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), position = 0)}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "equals", "COMMENT_NOTE"}), @Condition(type = Compare.class, params = {"my", "equals", "My"})}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"communicationEvent"})}), widgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyEditCommunicationEvent} ${parameters.communicationEventId}", id = "EditCommunicationEventPanel", includeForms = {
                    @IncludeForm(name = "EditInternalNote", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), failWidgets = @WidgetsForContainer(containers = {@Container2(style = "${styles.grid_row}", containers = {@Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyEditCommunicationEvent} ${parameters.communicationEventId} ${uiLabelMap.CommonFrom} ${communicationEvent.partyIdFrom}", id = "EditCommunicationEventPanel", includeForms = {
                    @IncludeForm(name = "EditInternalNote", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )})}), @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.PartyCommEventRoles}", includeForms = {
                    @IncludeForm(name = "ListCommRolesInline", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 0
                )}, sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "AddEventRole_submitAction", fromField = "AddEventRole_submitAction", defaultValue = "javascript:(document.AddEventRole.communicationEventId.value=document.EditInternalNote.communicationEventId.value),(document.AddEventRole.datetimeStarted.value=document.EditInternalNote.datetimeStarted.value),(document.AddEventRole.partyIdTo.value=document.EditInternalNote.partyIdTo.value),(document.AddEventRole.subject.value=document.EditInternalNote.subject.value),(document.AddEventRole.content.value=document.EditInternalNote.content.value),(document.AddEventRole.submit())"
                    )}), widgets = @WidgetsLeaf(includeForms = {
                        @IncludeForm(name = "AddEventRole", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                    )}), position = 1)}), @ScreenletNested(title = "${uiLabelMap.PartyCommContent}", includeForms = {
                    @IncludeForm(name = "listCommContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml", position = 0
                )}, sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "showProgress", value = "false", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "progressSuccessAction", value = "redirect;;MyCommunicationEvents?partyIdFrom=${partyId}&statusId=COM_PENDING&form=new&my=My&communicationEventTypeId=COMMENT_NOTE&communicationEventId=${communicationEvent.communicationEventId}&portalPageId=${parameters.portalPageId}"
                ),
                @Action(type = ActionType.SET, field = "progressOptions", value = "{}"
                ),
                @Action(type = ActionType.SET, field = "uploadContent_submitAction", value = "javascript:(document.uploadContent.datetimeStarted.value=document.EditInternalNote.datetimeStarted.value),(document.uploadContent.partyIdTo.value=document.EditInternalNote.partyIdTo.value),(document.uploadContent.subject.value=document.EditInternalNote.subject.value),(document.uploadContent.content.value=document.EditInternalNote.content.value),(jQuery(document.uploadContent).trigger('submit')),void(0)"
                )}), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "uploadContent", location = "component://party/widget/partymgr/CommunicationEventForms.xml"
                )}), position = 1)})})})}))}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = Empty.class, params = {"my"}), @ConditionNode(type = And.class), @ConditionNode(parent = 1, type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "not-equals", "COMMENT_NOTE"}), @ConditionNode(parent = 1, type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "not-equals", "EMAIL_COMMUNICATION"}), @ConditionNode(parent = 1, type = Compare.class, params = {"communicationEvent.communicationEventTypeId", "not-equals", "AUTO_EMAIL_COMM"})})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyEditCommunicationEvent} ${parameters.communicationEventId}", name = "EditCommunicationEventPanel", includeForms = {@IncludeForm(name = "EditCommEvent", location = "component://party/widget/partymgr/CommunicationEventForms.xml")}), @Screenlet(title = "${uiLabelMap.PartyOtherAndGeneralCommunicationEvents}", includeForms = {@IncludeForm(name = "ListChildCommEvents", location = "component://party/widget/partymgr/CommunicationEventForms.xml")})}))
    public interface editCommEvent {}

}
