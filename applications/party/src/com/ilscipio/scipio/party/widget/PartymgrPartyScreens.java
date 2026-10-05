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
public class PartymgrPartyScreens {

    @Screen(name = "findparty", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindParty")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findparty")
    @Action(type = ActionType.SERVICE, serviceName = "findParty")
    @Action(type = ActionType.SET, field = "searchPerformed", value = "${groovy: parameters.lookupFlag == 'Y'}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/findparty.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface findparty {}

    @Screen(name = "viewprofile", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PartyProfile}: ${partyId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/PartyProfileContent.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/ViewProfile.groovy")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(ifs = {
                    @IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"partyId"}),
                        @Condition(type = NotEmpty.class, params = {"parameters.telno"
                    })}), then = @Actions2(value = {
                        @Action(type = ActionType.SERVICE, serviceName = "findPartyFromTelephone", resultMapName = "telnoMap"
                    ),
                    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party", fieldMaps = {
                        @FieldMap(fieldName = "partyId", fromField = "telnoMap.partyId"
                    )}),
                    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "party.partyId"
                )})),
                @IfAction2(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"partyId"}),
                    @Condition(type = NotEmpty.class, params = {"parameters.email"
                })}), then = @Actions2(value = {
                    @Action(type = ActionType.SERVICE, serviceName = "findPartyFromEmailAddress", resultMapName = "emailMap", fieldMaps = {
                        @FieldMap(fieldName = "address", fromField = "parameters.email"
                    )}),
                    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party", fieldMaps = {
                        @FieldMap(fieldName = "partyId", fromField = "emailMap.partyId"
                    )}),
                    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "party.partyId"
                )}))}), widgets = @InlineWidgets(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"party"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "ProfileSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
                        )}, containers = {
                            @Container2(style = "${styles.grid_row}", containers = {
                                @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                    @IncludeScreen(name = "Party", location = "component://party/widget/partymgr/ProfileScreens.xml"
                                ),
                                @IncludeScreen(name = "LoyaltyPoints", location = "component://party/widget/partymgr/ProfileScreens.xml"
                            ),
                            @IncludeScreen(name = "UserLogin", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        )}),
                        @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "Contact", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        ),
                        @IncludeScreen(name = "Visits", location = "component://party/widget/partymgr/ProfileScreens.xml"
                    )})}),
                    @Container2(style = "${styles.grid_row}", containers = {
                        @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "PaymentMethods", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        ),
                        @IncludeScreen(name = "ScipioListUserCommunications", location = "component://party/widget/partymgr/ProfileScreens.xml"
                    )}),
                    @Container3(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "PartyIdentifications", location = "component://party/widget/partymgr/ProfileScreens.xml"
                    )})}),
                    @Container2(style = "${styles.grid_row}", containers = {
                        @Container3(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "Notes", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        )})})}), failWidgets = @WidgetsForContainer(containers = {
                            @Container2(labels = {
                                @Label(text = "${uiLabelMap.PartyNoPartyFoundWithPartyId}: ${parameters.partyId}", style = "common-msg-error"
                            )})}))}))})
        }
    )
    public interface viewprofile {}

    @Screen(name = "viewroles", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartyRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewroles")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyMemberRoles")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleTypeAndParty", list = "partyRoles", conditions = {@ConditionExpr(fieldName = "partyId", operator = "equals", value = "${parameters.partyId}"), @ConditionExpr(fieldName = "roleTypeId", operator = "not-equals", value = "_NA_")})
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyMemberRoles}", includeForms = {
                    @IncludeForm(name = "ViewPartyRoles", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = HasPermission.class, params = {"PARTYMGR", "_UPDATE"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PartyAddToRole}", containers = {
                            @Container(includeForms = {
                                @IncludeForm(name = "AddPartyMainRole", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                            )}, labels = {
                                @Label(text = "${uiLabelMap.PartyAddToMainRole}", style = "heading", position = 0
                            )}, containers = {
                                @Container2(id = "addPartySecondaryRole", position = 2)}),
                                @Container(includeForms = {
                                    @IncludeForm(name = "AddPartyRole", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                                )}, labels = {
                                    @Label(text = "${uiLabelMap.PartyAddToRoleViewAll}", style = "heading", position = 0
                                )})})})),
                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                                })}), actions = @Actions(value = {
                                    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleType", list = "parentRoleList", conditions = {
                                        @ConditionExpr(fieldName = "parentTypeId", operator = "equals", fromField = "nullField"
                                    )}, orderBy = {"description"})}), widgets = @InlineWidgets(screenlets = {
                                        @Screenlet(title = "${uiLabelMap.PartyNewRoleType}", includeForms = {
                                            @IncludeForm(name = "AddRoleType", location = "component://party/widget/partymgr/PartyForms.xml"
                                        )})}))})
        }
    )
    public interface viewroles {}

    @Screen(name = "AddPartySecondaryRoles", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyAddToSecondRole}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "AddPartySecondaryRoles", location = "component://party/widget/partymgr/PartyForms.xml")}))
    public interface AddPartySecondaryRoles {}

    @Screen(name = "linkparty", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyLink")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "linkparty")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_UPDATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "PartyLink", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                    )}, labels = {
                        @Label(text = "${uiLabelMap.PartyLinkExplanation}", position = 0
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface linkparty {}

    @Screen(name = "EditPartyRelationships", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyRelationships")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyRelationships")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyRelationships")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleType", list = "roleTypes", orderBy = {"description", "roleTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleTypeAndParty", list = "roleTypesForCurrentParty", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "partyId")}, orderBy = {"description", "roleTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRelationshipType", list = "relateTypes", orderBy = {"description", "partyRelationshipTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRelationship", list = "partyRelationships", conditions = {@ConditionExpr(fieldName = "partyIdTo", fromField = "partyId"), @ConditionExpr(fieldName = "partyIdFrom", fromField = "partyId")}, orderBy = {"partyIdTo", "partyRelationshipTypeId", "-fromDate"})
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", sections = {
                        @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                        })}), widgets = @WidgetsForContainer2(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.PartyNewRelationshipType}", includeForms = {
                    @IncludeForm(name = "AddPartyRelationshipType", location = "component://party/widget/partymgr/PartyForms.xml"
                
                        )})}))}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.PartyAddOtherRelationship}", includeForms = {
                    @IncludeForm(name = "AddOtherPartyRelationship", location = "component://party/widget/partymgr/PartyForms.xml"
                
                        )})})})}, screenlets = {
                            @Screenlet(title = "${uiLabelMap.PartyRelationships}", includeForms = {
                                @IncludeForm(name = "ListPartyRelationships", location = "component://party/widget/partymgr/PartyForms.xml"
                            )})})
        }
    )
    public interface EditPartyRelationships {}

    @Screen(name = "viewvendor", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewVendorParty")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewvendor")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyVendorInformation")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Vendor", valueField = "vendor")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditVendor", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface viewvendor {}

    @Screen(name = "EditPartyAttribute", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyAttribute")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewvendor")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyAttribute")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "attrName", fromField = "parameters.attrName")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAttribute", valueField = "attribute")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyAttribute}", includeForms = {
                    @IncludeForm(name = "EditPartyAttribute", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface EditPartyAttribute {}

    @Screen(name = "EditPartyTaxAuthInfos", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyTaxAuthInfos")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyTaxAuthInfos")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyTaxAuthInfos")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyTaxAuthInfo", list = "partyTaxInfos", fieldMaps = {@FieldMap(fieldName = "partyId")}, orderBy = {"taxAuthGeoId", "taxAuthPartyId", "fromDate"})
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdatePartyTaxAuthInfo", location = "component://party/widget/partymgr/PartyForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPartyTaxAuthInfos}", name = "AddPartyTaxAuthInfospanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyTaxAuthInfo", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyTaxAuthInfos {}

    @Screen(name = "editShoppingList", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShoppingList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editShoppingList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyShoppingLists")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/EditShoppingList.groovy")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editShoppingList.ftl"
            )})
        }
    )
    public interface editShoppingList {}

    @Screen(name = "editcontactmech", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContactMech")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "editcontactmech")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditContactMech")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/HasPartyPermissions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/EditContactMech.groovy")
    @Action(type = ActionType.SET, field = "dependentForm", value = "editcontactmechform")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", fromField = "mechMap.postalAddress.stateProvinceGeoId", defaultValue = "_none_")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Or.class, tree = {
                        @ConditionNode(type = True.class, params = {"hasViewPermission"
                    }),
                    @ConditionNode(type = True.class, params = {"hasPcmCreatePermission"
                }),
                @ConditionNode(type = True.class, params = {"hasPcmUpdatePermission"
            }),
            @ConditionNode(type = CompareField.class, params = {"parameters.partyId", "equals", "userLogin.partyId"
            })})}), widgets = @InlineWidgets(sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifEmpty = {"parameters.contactMechId"}, ifNotEmpty = {"mechMap.partyContactMech"
                })}), widgets = @WidgetsForContainer(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
                ),
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editcontactmech.ftl"
            )}), failWidgets = @WidgetsForContainer(containers = {
                @Container2(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "backHome"
                )}),
                @Container2(labels = {
                    @Label(text = "${uiLabelMap.PartyContactInfoNotBelongToPartyOrExpired}", style = "common-msg-error"
                )})}))}), failWidgets = @InlineWidgets(containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "backHome"
                    )}),
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.PartyMsgContactNotBelongToYou}", style = "common-msg-error-perm"
                    )})}))})
        }
    )
    public interface editcontactmech {}

    @Screen(name = "EditPerson", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditPersonalInformation")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAndPerson", valueField = "personInfo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.personInfo ? 'viewprofile' : 'newperson'}")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.personInfo ? 'PageTitleEditPersonalInformation' : 'PartyCreateNewPerson'}")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPerson", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface EditPerson {}

    @Screen(name = "EditPartyGroup", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditGroupInformation")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyAndGroup", valueField = "partyGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.partyGroup ? 'viewprofile' : 'newpartygroup'}")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.partyGroup ? 'PageTitleEditGroupInformation' : 'PartyCreateNewPartyGroup'}")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPartyGroup", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface EditPartyGroup {}

    @Screen(name = "EditUserLogin", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "UserLoginUpdateSecuritySettings")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "UserLoginUpdateSecuritySettings")
    @Action(type = ActionType.SET, field = "updateUserLoginSecurityURI", value = "ProfileUpdateUserLoginSecurity")
    @Action(type = ActionType.SET, field = "updatePasswordURI", value = "ProfileUpdatePassword")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "userLoginId", fromField = "parameters.userLoginId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UserLogin", valueField = "editUserLogin")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "component://common/widget/SecurityScreens.xml#updateUserLoginSecurity", location = "component://party/widget/partymgr/PartyScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.UserLoginChangePassword}", includeForms = {
                    @IncludeForm(name = "updatePassword", location = "component://common/widget/SecurityForms.xml"
                )})})
        }
    )
    public interface EditUserLogin {}

    @Screen(name = "CreateUserLogin", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CreateUserLogin")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "CreateUserLogin")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "createUserLoginURI", value = "ProfileCreateUserLogin")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddUserLogin", location = "component://common/widget/SecurityForms.xml"
                )})})
        }
    )
    public interface CreateUserLogin {}

    @Screen(name = "EditUserLoginSecurityGroups", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditUserLoginSecurityGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "addUserLoginSecurityGroupURI", value = "ProfileAddUserLoginToSecurityGroup")
    @Action(type = ActionType.SET, field = "removeUserLoginSecurityGroupURI", value = "ProfileRemoveUserLoginFromSecurityGroup")
    @Action(type = ActionType.SET, field = "updateUserLoginSecurityGroupURI", value = "ProfileUpdateUserLoginToSecurityGroup")
    @Action(type = ActionType.SET, field = "userLoginId", fromField = "parameters.userLoginId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UserLogin", valueField = "editUserLogin")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId", defaultValue = "${editUserLogin.partyId}")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUserLoginSecurityGroups", location = "component://common/widget/SecurityForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AddUserLoginToSecurityGroup}", name = "AddUserLoginSecurityGroupsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddUserLoginSecurityGroup", location = "component://common/widget/SecurityForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditUserLoginSecurityGroups {}

    @Screen(name = "AddPartyNote", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNewPartyNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewprofile")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleNewPartyNote")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "viewprofile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddPartyNote", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface AddPartyNote {}

    @Screen(name = "EditPartyRates", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyRates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyRates")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditPartyRates")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyRates", location = "component://party/widget/partymgr/PartyForms.xml"
            )}, screenlets = {
                @Screenlet(name = "AddPartyRatesPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyRate", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyRates {}

    @Screen(name = "ViewSegmentRoles", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartySegmentRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewSegmentRoles")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ViewSegmentRoles")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSegmentRoles", location = "component://party/widget/partymgr/PartyForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPartySegmentRoles}", name = "AddSegmentRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSegmentRole", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, position = 0)})
        }
    )
    public interface ViewSegmentRoles {}

    @Screen(name = "CreateNewParty", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateNewPartyDetail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "find")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeMenus = {
                        @IncludeMenu(name = "create-new-party", location = "component://party/widget/partymgr/PartyMenus.xml", position = 1
                    )}, labels = {
                        @Label(text = "${uiLabelMap.PartySelectTypePartyToCreate}:", position = 0
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CreateNewParty {}

    @Screen(name = "NewCustomer", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyCreateNewCustomer")
    @Action(type = ActionType.SET, field = "target", value = "createCustomer")
    @Action(type = ActionType.SET, field = "displayPassword", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "previousParams", fromField = "_PREVIOUS_PARAMS_", fromScope = "user")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newcustomer")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "dependentForm", value = "NewUser"
                ),
                @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "mainId", value = "USER_COUNTRY"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "USER_STATE"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            ),
            @Action(type = ActionType.SET, field = "focusFieldName", value = "NewUser_USER_PARTY_ID"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewUser", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface NewCustomer {}

    @Screen(name = "NewProspect", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyCreateNewProspect")
    @Action(type = ActionType.SET, field = "displayPassword", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "target", value = "createProspect")
    @Action(type = ActionType.SET, field = "previousParams", fromField = "_PREVIOUS_PARAMS_", fromScope = "user")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newprospect")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "dependentForm", value = "NewUser"
                ),
                @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "mainId", value = "USER_COUNTRY"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "USER_STATE"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewUser", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface NewProspect {}

    @Screen(name = "NewEmployee", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyCreateNewEmployee")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "newemployee")
    @Action(type = ActionType.SET, field = "displayPassword", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "target", value = "createEmployee")
    @Action(type = ActionType.SET, field = "previousParams", fromField = "_PREVIOUS_PARAMS_", fromScope = "user")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "dependentForm", value = "NewUser"
                ),
                @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "mainId", value = "USER_COUNTRY"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "USER_STATE"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewUser", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface NewEmployee {}

    @Screen(name = "EditPartyContents", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "titleProperty", fromField = "titleProperty", defaultValue = "PageTitleListContent")
    @Action(order = 1, type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "PartyContents")
    @Action(order = 2, type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(order = 3, type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(order = 4, type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @IfAction(order = 5, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"content"})}), then = @Actions(value = {@Action(order = 0, type = ActionType.ENTITY_AND, entityName = "PartyContent", list = "partyContentList", fieldMaps = {@FieldMap(fieldName = "partyId"), @FieldMap(fieldName = "contentId")})}, ifs = {@IfAction2(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"partyContentList"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "content", valueType = "Object")}))}))
    @IfAction(order = 6, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"partyId"}), @Condition(type = NotEmpty.class, params = {"contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.fromDate"}), @Condition(type = NotEmpty.class, params = {"parameters.partyContentTypeId"})}), then = @Actions(value = {@Action(type = ActionType.ENTITY_ONE, entityName = "PartyContent", valueField = "partyContent", fieldMaps = {@FieldMap(fieldName = "partyId"), @FieldMap(fieldName = "contentId"), @FieldMap(fieldName = "partyContentTypeId", fromField = "parameters.partyContentTypeId"), @FieldMap(fieldName = "fromDate", fromField = "parameters.fromDate")})}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "partyContent", valueType = "Object")}))
    @Action(order = 7, type = ActionType.SET, field = "skipProfileHeader", value = "true", valueType = "Boolean")
    @Action(order = 8, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle}${groovy: context.partyId ? (': ' + context.partyId) : ''}")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "paginateTarget", value = "EditPartyContents?partyId=${parameters.partyId}"
                )}), includeForms = {
                    @IncludeForm(name = "ListPartyContents", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}, sections = {
                    @InlineSection(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "showProgress", value = "true", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "progressSuccessAction", value = "redirect;;EditPartyContents?partyId=${parameters.partyId}"
                ),
                @Action(type = ActionType.SET, field = "progressOptions", value = "{}"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${groovy: context.content ? uiLabelMap.PageTitleEditPartyContent : uiLabelMap.PageTitleAddPartyContent}", includeForms = {
                    @IncludeForm(name = "AddPartyContent", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "PartyContentAddEditSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml", position = 0
                )})}))})
        }
    )
    public interface EditPartyContents {}

    @Screen(name = "editCarrierAccount", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePartyCarrierAccount")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyShipperAccount}", includeForms = {
                    @IncludeForm(name = "EditCarrierAccount", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})
        }
    )
    public interface editCarrierAccount {}

    @Screen(name = "EditPartyResumes", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditPartyResume")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyResumes")
    @Action(type = ActionType.SET, field = "resumeId", fromField = "parameters.resumeId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyResume", valueField = "partyResume")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyResumes", location = "component://humanres/widget/forms/PartyResumeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.PartyParty} ${uiLabelMap.HumanResPartyResume}", name = "EditPartyResumePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditPartyResume", location = "component://humanres/widget/forms/PartyResumeForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyResumes {}

    @Screen(name = "PartyFinancialHistory", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyFinancialHistory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FinancialHistory")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceAndApplAndPayment", list = "ListInvoicesApplPayments", conditions = {
                        @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "INVOICE_IN_PROCESS"
                    ),
                    @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "INVOICE_CANCELLED"
                ),
                @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "INVOICE_WRITEOFF"
            )})}), widgets = @InlineWidgets(sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"ListInvoicesApplPayments"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "actualCurrency", value = "false", valueType = "Boolean"
                ),
                @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "defaultOrganizationPartyCurrencyUomId"
            )}), widgets = @WidgetsForContainer(screenlets = {
                @ScreenletNested(title = "${uiLabelMap.AccountingInvoicesApplPayments}", navigationFormName = "Invoices", includeForms = {
                    @IncludeForm(name = "ListInvoicesApplPayments", location = "component://party/widget/partymgr/PartyForms.xml", position = 0
                
            )}, sections = {
                    @SectionLeaf(condition = @Condition(type = And.class, tree = {
                        @ConditionNode(type = And.class
            ),
                        @ConditionNode(parent = 0, not = true, type = Empty.class, params = {"party.preferredCurrencyUomId"
                    
            }),
                    @ConditionNode(parent = 0, type = CompareField.class, params = {"defaultOrganizationPartyCurrencyId", "not-equals", "party.preferredCurrencyUomId"
                
            })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "actualCurrency", value = "true", valueType = "Boolean"
                
            ),
                @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "party.preferredCurrencyUomId"
                
            )}), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "ListInvoicesApplPayments", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                
            )}, labels = {
                    @Label(text = "${uiLabelMap.PartyCurrency}", style = "heading", position = 0
                
            )}), position = 1)})}))})),
            @InlineSection(actions = @Actions(value = {
                @Action(type = ActionType.SET, field = "actualCurrency", value = "false", valueType = "Boolean"
            ),
            @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "defaultOrganizationPartyCurrencyUomId"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyInvoicesNotApplied}", includeForms = {
                    @IncludeForm(name = "ListUnAppliedInvoices", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"party.preferredCurrencyUomId"
                    }),
                    @Condition(type = CompareField.class, params = {"defaultOrganizationPartyCurrencyUomId", "not-equals", "party.preferredCurrencyUomId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "actualCurrency", value = "true", valueType = "Boolean"
                ),
                @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "party.preferredCurrencyUomId"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyCurrency}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUnAppliedInvoices", location = "component://party/widget/partymgr/PartyForms.xml"
            )}))})})),
            @InlineSection(actions = @Actions(value = {
                @Action(type = ActionType.SET, field = "actualCurrency", value = "false", valueType = "Boolean"
            ),
            @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "defaultOrganizationPartyCurrencyId"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyPaymentsNotApplied}", includeForms = {
                    @IncludeForm(name = "ListUnAppliedPayments", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"party.preferredCurrencyUomId"
                    }),
                    @Condition(type = CompareField.class, params = {"defaultOrganizationPartyCurrencyId", "not-equals", "party.preferredCurrencyUomId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "actualCurrency", value = "true", valueType = "Boolean"
                ),
                @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "party.preferredCurrencyUomId"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyCurrency}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUnAppliedPayments", location = "component://party/widget/partymgr/PartyForms.xml"
            )}))})})),
            @InlineSection(actions = @Actions(value = {
                @Action(type = ActionType.SET, field = "actualCurrency", value = "false", valueType = "Boolean"
            ),
            @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "defaultOrganizationPartyCurrencyUomId"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.PartyFinancialSummary}${defaultOrganizationPartyId}", includeForms = {
                    @IncludeForm(name = "partyFinancialSummary", location = "component://party/widget/partymgr/PartyForms.xml"
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"party.preferredCurrencyUomId"
                    }),
                    @Condition(type = CompareField.class, params = {"defaultOrganizationPartyCurrencyUomId", "not-equals", "party.preferredCurrencyUomId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "actualCurrency", value = "true", valueType = "Boolean"
                ),
                @Action(type = ActionType.SET, field = "actualCurrencyUomId", fromField = "party.preferredCurrencyUomId"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyCurrency}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "partyFinancialSummary", location = "component://party/widget/partymgr/PartyForms.xml"
            )}))})})),
            @InlineSection(actions = @Actions(value = {
                @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/payment/BillingAccounts.groovy"
            )}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingBillingAccount}", includeForms = {
                    @IncludeForm(name = "PartyBillingAccount", location = "component://party/widget/partymgr/PartyForms.xml"
                )})})),
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.ENTITY_AND, entityName = "ReturnHeader", list = "returnList", fieldMaps = {
                        @FieldMap(fieldName = "fromPartyId", fromField = "parameters.partyId"
                    )})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.OrderOrderReturns}", includeForms = {
                            @IncludeForm(name = "PartyReturns", location = "component://party/widget/partymgr/PartyForms.xml"
                        )})}))})
        }
    )
    public interface PartyFinancialHistory {}

    @Screen(name = "Preferences", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartyPreferences")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "preferences")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "EnumTypeChildAndEnum", list = "enumTypeChildAndEnums", fieldMaps = {@FieldMap(fieldName = "parentEnumTypeId", value = "USER_PREF_GROUPS")})
    @Action(type = ActionType.ENTITY_AND, entityName = "UserLogin", list = "userLogins", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.partyId")})
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.ITERATE_SECTION, list = "userLogins", entry = "userLogin", name = "Preferences-iterate1", location = "component://party/widget/partymgr/PartyScreens.xml"
            )})
        }
    )
    public interface Preferences {}

    @Screen(name = "Preferences-iterate1", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "userPrefUserLoginId", fromField = "userLogin.userLoginId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonPreferences} ${uiLabelMap.CommonFor} userLogin: ${userPrefUserLoginId}", includeForms = {@IncludeForm(name = "ListPreference", location = "component://party/widget/partymgr/PartyForms.xml")})}))
    public interface Preferences_iterate1 {}

    @Screen(name = "PartyGeoLocation", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePartyGeoLocation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyGeoLocation")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/PartyGeoLocation.groovy")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"geoChart"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLatitude} ${latestGeoPoint.latitude}", position = 1
                    ),
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLongitude} ${latestGeoPoint.longitude}", position = 2
                ),
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonElevation} ${latestGeoPoint.elevation} ${elevationUomAbbr}", position = 3
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml", position = 4
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonUpdate}", style = "${styles.link_nav} ${styles.action_update}", target = "addGeoLocation"
                )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "addGeoLocation"
                    )}, position = 0)}))})
        }
    )
    public interface PartyGeoLocation {}

    @Screen(name = "GetPartyGeoLocation", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePartyGeoLocation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyGeoLocation")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/PartyGeoLocation.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLatitude} ${latestGeoPoint.latitude}"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLongitude} ${latestGeoPoint.longitude}"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonElevation} ${latestGeoPoint.elevation} ${elevationUomAbbr}"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml")}))
    public interface GetPartyGeoLocation {}

    @Screen(name = "EditGeoLocation", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePartyGeoLocation")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyGeoLocation")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetGeoLocation.groovy")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/editGeoLocation.ftl"
            )})
        }
    )
    public interface EditGeoLocation {}

    @Screen(name = "CreateUserNotification", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MyPortalCustRequestNotificationMailCreation}")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/templates/email/CreatePartyNotification.ftl", platform = "email"
            )})
        }
    )
    public interface CreateUserNotification {}

    @Screen(name = "ListPartyIdentifications", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListPartyIdentifications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewidentifications")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyIdentification")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyIdentification", valueField = "partyIdentification")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "listPartyIdentification", location = "component://party/widget/partymgr/PartyForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PartyPartyIdentification}", name = "PartyIdentificationCreationPanel", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "partyIdentification", value = "${null}"
                ),
                @Action(type = ActionType.SET, field = "useRequestParameters", value = "false", valueType = "Boolean"
            )}), includeForms = {
                @IncludeForm(name = "editPartyIdentification", location = "component://party/widget/partymgr/PartyForms.xml"
            )})})
        }
    )
    public interface ListPartyIdentifications {}

    @Screen(name = "ViewProductStoreRoles", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindProductStoreRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productStoreRoles")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreRoles")
    @Action(type = ActionType.SET, field = "parameters.fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreRole", valueField = "productStoreRole")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreRoles}", includeForms = {
                    @IncludeForm(name = "EditProductStoreRole", location = "component://party/widget/partymgr/PartyForms.xml"
                )})}, decorators = {
                    @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProductStoreRole", location = "component://party/widget/partymgr/PartyForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductStoreRole", location = "component://party/widget/partymgr/PartyForms.xml"
                        )}))})})
        }
    )
    public interface ViewProductStoreRoles {}

    @Screen(name = "postalAddressHtmlFormatter", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "postalAddressTemplateSuffix", value = ".ftl")
    @Action(type = ActionType.SET, field = "addressTemplatePath", value = "${sys:getProperty('ofbiz.home')}/applications/party/webapp/partymgr/party/contactmechtemplates/")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetPostalAddressTemplate.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/contactmechtemplates/${postalAddressTemplate}")}))
    public interface postalAddressHtmlFormatter {}

    @Screen(name = "postalAddressPdfFormatter", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "postalAddressTemplateSuffix", value = ".fo.ftl")
    @Action(type = ActionType.SET, field = "addressTemplatePath", value = "${sys:getProperty('ofbiz.home')}/applications/party/webapp/partymgr/party/contactmechtemplates/")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetPostalAddressTemplate.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://party/webapp/partymgr/party/contactmechtemplates/${postalAddressTemplate}", platform = "xsl-fo")}))
    public interface postalAddressPdfFormatter {}

    @Screen(name = "ImportExport", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonImportExport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "importexport")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.PartyParty} ${uiLabelMap.CommonImportExport} ID Name, single role (employee, customer, supplier) and contactmechs"
                )}, containers = {
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", labels = {
                            @Label(text = "${uiLabelMap.CommonImport}", style = "heading"
                        )}, sections = {
                            @SectionNested2(actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "showProgress", value = "true", valueType = "Boolean"
                            ),
                            @Action(type = ActionType.SET, field = "progressSuccessAction", value = "none"
                        ),
                        @Action(type = ActionType.SET, field = "progressOptions", value = "{}"
                    )}), widgets = @WidgetsForContainer2(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ImportParty", location = "component://party/widget/partymgr/PartyForms.xml"
                    )}))}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeForms = {
                        @IncludeForm(name = "ExportParty", location = "component://party/widget/partymgr/PartyForms.xml", position = 1
                    )}, labels = {
                        @Label(text = "${uiLabelMap.CommonExport}", style = "heading", position = 0
                    )})})})})
        }
    )
    public interface ImportExport {}

    @Screen(name = "PartyExportCsv", location = "component://party/widget/partymgr/PartyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ExportPartyCsv", location = "component://party/widget/partymgr/PartyForms.xml")}))
    public interface PartyExportCsv {}

}
