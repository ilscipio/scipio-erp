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
package com.ilscipio.scipio.party.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.common.CommonEvents;
// NOTE: ShoppingListEvents is in the order module which party does not depend on; use reflection
// import org.ofbiz.order.shoppinglist.ShoppingListEvents;
import org.ofbiz.content.data.DataEvents;
import org.ofbiz.party.communication.CommunicationEventServices;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartymgrControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://party/widget/partymgr/CommonScreens.xml#main",
        controller = "partymgr"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "findparty",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#findparty",
        controller = "partymgr"
    )
    public static final String VIEW_FINDPARTY = "findparty";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewprofile",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#viewprofile",
        controller = "partymgr"
    )
    public static final String VIEW_VIEWPROFILE = "viewprofile";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "partyContentList",
        type = "screen",
        page = "component://party/widget/partymgr/ProfileScreens.xml#ContentList",
        controller = "partymgr"
    )
    public static final String VIEW_PARTYCONTENTLIST = "partyContentList";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewroles",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#viewroles",
        controller = "partymgr"
    )
    public static final String VIEW_VIEWROLES = "viewroles";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "addsecondaryroles",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#AddPartySecondaryRoles",
        controller = "partymgr"
    )
    public static final String VIEW_ADDSECONDARYROLES = "addsecondaryroles";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewidentifications",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#ListPartyIdentifications",
        controller = "partymgr"
    )
    public static final String VIEW_VIEWIDENTIFICATIONS = "viewidentifications";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "linkparty",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#linkparty",
        controller = "partymgr"
    )
    public static final String VIEW_LINKPARTY = "linkparty";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditPartyRelationships",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyRelationships",
        controller = "partymgr"
    )
    public static final String VIEW_EDITPARTYRELATIONSHIPS = "EditPartyRelationships";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewvendor",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#viewvendor",
        controller = "partymgr"
    )
    public static final String VIEW_VIEWVENDOR = "viewvendor";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditPartyTaxAuthInfos",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyTaxAuthInfos",
        controller = "partymgr"
    )
    public static final String VIEW_EDITPARTYTAXAUTHINFOS = "EditPartyTaxAuthInfos";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editShoppingList",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#editShoppingList",
        controller = "partymgr"
    )
    public static final String VIEW_EDITSHOPPINGLIST = "editShoppingList";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupUserLogin",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
        controller = "partymgr"
    )
    public static final String VIEW_LOOKUPUSERLOGIN = "LookupUserLogin";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ProfileEditUserLogin",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditUserLogin",
        controller = "partymgr"
    )
    public static final String VIEW_PROFILEEDITUSERLOGIN = "ProfileEditUserLogin";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ProfileCreateNewLogin",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#CreateUserLogin",
        controller = "partymgr"
    )
    public static final String VIEW_PROFILECREATENEWLOGIN = "ProfileCreateNewLogin";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ProfileEditUserLoginSecurityGroups",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditUserLoginSecurityGroups",
        controller = "partymgr"
    )
    public static final String VIEW_PROFILEEDITUSERLOGINSECURITYGROUPS = "ProfileEditUserLoginSecurityGroups";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditPerson",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditPerson",
        controller = "partymgr"
    )
    public static final String VIEW_EDITPERSON = "EditPerson";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditPartyGroup",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyGroup",
        controller = "partymgr"
    )
    public static final String VIEW_EDITPARTYGROUP = "EditPartyGroup";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditPartyAttribute",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyAttribute",
        controller = "partymgr"
    )
    public static final String VIEW_EDITPARTYATTRIBUTE = "EditPartyAttribute";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "AddPartyNote",
        type = "screen",
        page = "component://party/widget/partymgr/PartyScreens.xml#AddPartyNote",
        controller = "partymgr"
    )
    public static final String VIEW_ADDPARTYNOTE = "AddPartyNote";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewSegmentRoles",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#ViewSegmentRoles",
            controller = "partymgr"
        )
        public static final String VIEW_VIEWSEGMENTROLES = "ViewSegmentRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListPartyContactLists",
            type = "screen",
            page = "component://party/widget/partymgr/PartyContactListScreens.xml#ListPartyContactLists",
            controller = "partymgr"
        )
        public static final String VIEW_LISTPARTYCONTACTLISTS = "ListPartyContactLists";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyRates",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyRates",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYRATES = "EditPartyRates";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcontactmech",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#editcontactmech",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCONTACTMECH = "editcontactmech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcreditcard",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#editcreditcard",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCREDITCARD = "editcreditcard";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editgiftcard",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#editgiftcard",
            controller = "partymgr"
        )
        public static final String VIEW_EDITGIFTCARD = "editgiftcard";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editeftaccount",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#editeftaccount",
            controller = "partymgr"
        )
        public static final String VIEW_EDITEFTACCOUNT = "editeftaccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListCommContent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#ListCommContent",
            controller = "partymgr"
        )
        public static final String VIEW_LISTCOMMCONTENT = "ListCommContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PendingCommunications",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#PendingCommunications",
            controller = "partymgr"
        )
        public static final String VIEW_PENDINGCOMMUNICATIONS = "PendingCommunications";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListPartyCommEvents",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#ListPartyCommEvents",
            controller = "partymgr"
        )
        public static final String VIEW_LISTPARTYCOMMEVENTS = "ListPartyCommEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListUnknownPartyComms",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#ListUnknownPartyComms",
            controller = "partymgr"
        )
        public static final String VIEW_LISTUNKNOWNPARTYCOMMS = "ListUnknownPartyComms";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindCommunicationByOrder",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#FindCommunicationByOrder",
            controller = "partymgr"
        )
        public static final String VIEW_FINDCOMMUNICATIONBYORDER = "FindCommunicationByOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "MyCommunicationEvents",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#PartyCommunicationEvents",
            controller = "partymgr"
        )
        public static final String VIEW_MYCOMMUNICATIONEVENTS = "MyCommunicationEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindCommunicationEvents",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#FindCommunicationEvents",
            controller = "partymgr"
        )
        public static final String VIEW_FINDCOMMUNICATIONEVENTS = "FindCommunicationEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCommunicationEvent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#EditCommunicationEvent",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCOMMUNICATIONEVENT = "EditCommunicationEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewCommunicationEvent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#ViewCommunicationEvent",
            controller = "partymgr"
        )
        public static final String VIEW_VIEWCOMMUNICATIONEVENT = "ViewCommunicationEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdateCommPurposes",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#UpdateCommPurposes",
            controller = "partymgr"
        )
        public static final String VIEW_UPDATECOMMPURPOSES = "UpdateCommPurposes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdateCommRoles",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#UpdateCommRoles",
            controller = "partymgr"
        )
        public static final String VIEW_UPDATECOMMROLES = "UpdateCommRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListCommWorkEfforts",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#ListCommWorkEfforts",
            controller = "partymgr"
        )
        public static final String VIEW_LISTCOMMWORKEFFORTS = "ListCommWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddCommEventWorkEffort",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#AddCommEventWorkEffort",
            controller = "partymgr"
        )
        public static final String VIEW_ADDCOMMEVENTWORKEFFORT = "AddCommEventWorkEffort";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCommEventWorkEffort",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#EditCommEventWorkEffort",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCOMMEVENTWORKEFFORT = "EditCommEventWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddCommContent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#AddCommContent",
            controller = "partymgr"
        )
        public static final String VIEW_ADDCOMMCONTENT = "AddCommContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCommContent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#EditCommContent",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCOMMCONTENT = "EditCommContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequestFromCommEvent",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#EditRequestFromCommEvent",
            controller = "partymgr"
        )
        public static final String VIEW_EDITREQUESTFROMCOMMEVENT = "EditRequestFromCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#ViewRequest",
            controller = "partymgr"
        )
        public static final String VIEW_VIEWREQUEST = "ViewRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditRequest",
            controller = "partymgr"
        )
        public static final String VIEW_EDITREQUEST = "EditRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateNewParty",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#CreateNewParty",
            controller = "partymgr"
        )
        public static final String VIEW_CREATENEWPARTY = "CreateNewParty";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewCustomer",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#NewCustomer",
            controller = "partymgr"
        )
        public static final String VIEW_NEWCUSTOMER = "NewCustomer";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewProspect",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#NewProspect",
            controller = "partymgr"
        )
        public static final String VIEW_NEWPROSPECT = "NewProspect";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewEmployee",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#NewEmployee",
            controller = "partymgr"
        )
        public static final String VIEW_NEWEMPLOYEE = "NewEmployee";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyClassifications",
            type = "screen",
            page = "component://party/widget/partymgr/PartyClassificationScreens.xml#EditPartyClassifications",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYCLASSIFICATIONS = "EditPartyClassifications";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyClassificationGroupParties",
            type = "screen",
            page = "component://party/widget/partymgr/PartyClassificationScreens.xml#EditPartyClassificationGroupParties",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYCLASSIFICATIONGROUPPARTIES = "EditPartyClassificationGroupParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPartyClassificationGroups",
            type = "screen",
            page = "component://party/widget/partymgr/PartyClassificationScreens.xml#FindPartyClassificationGroups",
            controller = "partymgr"
        )
        public static final String VIEW_FINDPARTYCLASSIFICATIONGROUPS = "FindPartyClassificationGroups";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyClassificationGroup",
            type = "screen",
            page = "component://party/widget/partymgr/PartyClassificationScreens.xml#EditPartyClassificationGroup",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYCLASSIFICATIONGROUP = "EditPartyClassificationGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findVisits",
            type = "screen",
            page = "component://party/widget/partymgr/VisitScreens.xml#FindVisits",
            controller = "partymgr"
        )
        public static final String VIEW_FINDVISITS = "findVisits";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "visitdetail",
            type = "screen",
            page = "component://party/widget/partymgr/VisitScreens.xml#visitdetail",
            controller = "partymgr"
        )
        public static final String VIEW_VISITDETAIL = "visitdetail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "listLoggedInUsers",
            type = "screen",
            page = "component://party/widget/partymgr/VisitScreens.xml#ListLoggedInUsers",
            controller = "partymgr"
        )
        public static final String VIEW_LISTLOGGEDINUSERS = "listLoggedInUsers";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyEmail",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyEmail",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPARTYEMAIL = "LookupPartyEmail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyClassificationGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyClassificationGroup",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPARTYCLASSIFICATIONGROUP = "LookupPartyClassificationGroup";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContact",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupContact",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPCONTACT = "LookupContact";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupLead",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupLead",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPLEAD = "LookupLead";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyAndUserLoginAndPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyAndUserLoginAndPerson",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPARTYANDUSERLOGINANDPERSON = "LookupPartyAndUserLoginAndPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupUserLoginAndPartyDetails",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPUSERLOGINANDPARTYDETAILS = "LookupUserLoginAndPartyDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyGroup",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPARTYGROUP = "LookupPartyGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAccount",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupAccount",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPACCOUNT = "LookupAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCommEvent",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCommEvent",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPCOMMEVENT = "LookupCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupCustRequest",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPCUSTREQUEST = "LookupCustRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPreferredContactMech",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupPreferredContactMech",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPREFERREDCONTACTMECH = "LookupPreferredContactMech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactList",
            type = "screen",
            page = "component://party/widget/partymgr/PartyContactListScreens.xml#ListLookupContactList",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPCONTACTLIST = "LookupContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#LookupDataResource",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPDATARESOURCE = "LookupDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupEmploymentApp",
            type = "screen",
            page = "component://humanres/widget/LookupScreens.xml#LookupEmploymentApp",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPEMPLOYMENTAPP = "LookupEmploymentApp";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupEmplPosition",
            type = "screen",
            page = "component://humanres/widget/LookupScreens.xml#LookupEmplPosition",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPEMPLPOSITION = "LookupEmplPosition";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSegmentGroup",
            type = "screen",
            page = "component://marketing/widget/LookupScreens.xml#LookupSegmentGroup",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPSEGMENTGROUP = "LookupSegmentGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "partymgr"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewSimpleContent",
            type = "simplecontent",
            controller = "partymgr"
        )
        public static final String VIEW_VIEWSIMPLECONTENT = "ViewSimpleContent";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ImportExport",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#ImportExport",
            controller = "partymgr"
        )
        public static final String VIEW_IMPORTEXPORT = "ImportExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyExportCsv",
            type = "screencsv",
            page = "component://party/widget/partymgr/PartyScreens.xml#PartyExportCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYEXPORTCSV = "PartyExportCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyContents",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyContents",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYCONTENTS = "EditPartyContents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editCarrierAccount",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#editCarrierAccount",
            controller = "partymgr"
        )
        public static final String VIEW_EDITCARRIERACCOUNT = "editCarrierAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "partyInvitation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyInvitationScreens.xml#FindPartyInvitations",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYINVITATION = "partyInvitation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editPartyInvitation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyInvitationScreens.xml#EditPartyInvitation",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYINVITATION = "editPartyInvitation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyInvitationGroupAssocs",
            type = "screen",
            page = "component://party/widget/partymgr/PartyInvitationScreens.xml#EditPartyInvitationsGroupAssocs",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYINVITATIONGROUPASSOCS = "PartyInvitationGroupAssocs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyInvitationRoleAssocs",
            type = "screen",
            page = "component://party/widget/partymgr/PartyInvitationScreens.xml#EditPartyInvitationsRoleAssocs",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYINVITATIONROLEASSOCS = "PartyInvitationRoleAssocs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartySkills",
            type = "screen",
            page = "component://humanres/widget/PartySkillScreens.xml#EditPartySkills",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYSKILLS = "EditPartySkills";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyResumes",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyResumes",
            controller = "partymgr"
        )
        public static final String VIEW_EDITPARTYRESUMES = "EditPartyResumes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmploymentApps",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#EditEmploymentApps",
            controller = "partymgr"
        )
        public static final String VIEW_EDITEMPLOYMENTAPPS = "EditEmploymentApps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyFinancialHistory",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#PartyFinancialHistory",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYFINANCIALHISTORY = "PartyFinancialHistory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "Preferences",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#Preferences",
            controller = "partymgr"
        )
        public static final String VIEW_PREFERENCES = "Preferences";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyGeoLocation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#PartyGeoLocation",
            controller = "partymgr"
        )
        public static final String VIEW_PARTYGEOLOCATION = "PartyGeoLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GetPartyGeoLocation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#GetPartyGeoLocation",
            controller = "partymgr"
        )
        public static final String VIEW_GETPARTYGEOLOCATION = "GetPartyGeoLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdateCommOrders",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#UpdateCommOrders",
            controller = "partymgr"
        )
        public static final String VIEW_UPDATECOMMORDERS = "UpdateCommOrders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdateCommProducts",
            type = "screen",
            page = "component://party/widget/partymgr/CommunicationEventScreens.xml#UpdateCommProducts",
            controller = "partymgr"
        )
        public static final String VIEW_UPDATECOMMPRODUCTS = "UpdateCommProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductStoreRoles",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#ViewProductStoreRoles",
            controller = "partymgr"
        )
        public static final String VIEW_VIEWPRODUCTSTOREROLES = "ViewProductStoreRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditBillingAccount",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#EditBillingAccount",
            controller = "partymgr"
        )
        public static final String VIEW_EDITBILLINGACCOUNT = "EditBillingAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "addGeoLocation",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#EditGeoLocation",
            controller = "partymgr"
        )
        public static final String VIEW_ADDGEOLOCATION = "addGeoLocation";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "view",
            controller = "partymgr",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface View {}

        @Request(
            uri = "main",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "viewprofile",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile", saveHomeView = "true")
        public interface Viewprofile {}

        @Request(
            uri = "EditPartyRelationships",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRelationships")
        public interface EditPartyRelationships {}

        @Request(
            uri = "viewroles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewroles")
        public interface Viewroles {}

        @Request(
            uri = "viewidentifications",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewidentifications")
        public interface Viewidentifications {}

        @Request(
            uri = "linkparty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "linkparty")
        public interface Linkparty {}

        @Request(
            uri = "partyInvitation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "partyInvitation")
        public interface PartyInvitation {}

        @Request(
            uri = "editPartyInvitation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPartyInvitation")
        public interface EditPartyInvitation {}

        @Request(
            uri = "PartyInvitationGroupAssocs",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationGroupAssocs")
        public interface PartyInvitationGroupAssocs {}

        @Request(
            uri = "PartyInvitationRoleAssocs",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationRoleAssocs")
        public interface PartyInvitationRoleAssocs {}

        @Request(
            uri = "createPartyInvitation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPartyInvitation")
        @Response(name = "error", type = "view", value = "editPartyInvitation")
        @Event(type = "service", invoke = "createPartyInvitation")
        public static String createPartyInvitation(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyInvitation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPartyInvitation")
        @Response(name = "error", type = "view", value = "editPartyInvitation")
        @Event(type = "service", invoke = "updatePartyInvitation")
        public static String updatePartyInvitation(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyInvitation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "partyInvitation")
        @Response(name = "error", type = "view", value = "partyInvitation")
        @Event(type = "service", invoke = "deletePartyInvitation")
        public static String deletePartyInvitation(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyInvitationGroupAssoc",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationGroupAssocs")
        @Response(name = "error", type = "view", value = "PartyInvitationGroupAssocs")
        @Event(type = "service", invoke = "createPartyInvitationGroupAssoc")
        public static String createPartyInvitationGroupAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyInvitationRoleAssoc",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationRoleAssocs")
        @Response(name = "error", type = "view", value = "PartyInvitationRoleAssocs")
        @Event(type = "service", invoke = "createPartyInvitationRoleAssoc")
        public static String createPartyInvitationRoleAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyInvitationGroupAssoc",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationGroupAssocs")
        @Response(name = "error", type = "view", value = "PartyInvitationGroupAssocs")
        @Event(type = "service", invoke = "deletePartyInvitationGroupAssoc")
        public static String deletePartyInvitationGroupAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyInvitationRoleAssoc",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyInvitationRoleAssocs")
        @Response(name = "error", type = "view", value = "PartyInvitationRoleAssocs")
        @Event(type = "service", invoke = "deletePartyInvitationRoleAssoc")
        public static String deletePartyInvitationRoleAssoc(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setPartyLink",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "linkPartyRecord")
        public static String setPartyLink(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "applyServiceCredit",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "createServiceCredit")
        public static String applyServiceCredit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "editcontactmech",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech", saveCurrentView = "true")
        public interface Editcontactmech {}

        @Request(
            uri = "createContactMech",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyContactMech")
        public static String createContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactMech",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyContactMech")
        public static String updateContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteContactMech",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "deletePartyContactMech")
        public static String deleteContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddressAndPurpose",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddressAndPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddress",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePostalAddress",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyPostalAddress")
        public static String updatePostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createTelecomNumber",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyTelecomNumber")
        public static String createTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTelecomNumber",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyTelecomNumber")
        public static String updateTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEmailAddress",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyEmailAddress")
        public static String createEmailAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmailAddress",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyEmailAddress")
        public static String updateEmailAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyContactMechPurpose",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyContactMechPurpose")
        public static String createPartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyContactMechPurpose",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "deletePartyContactMechPurpose")
        public static String deletePartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        public interface EditShoppingList {}

        @Request(
            uri = "createEmptyShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        @Event(type = "service", invoke = "createShoppingList")
        public static String createEmptyShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        @Event(type = "service", invoke = "updateShoppingList")
        public static String updateShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addItemToShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        @Event(type = "service", invoke = "createShoppingListItem")
        public static String addItemToShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addBulkToShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String addBulkToShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addBulkFromCart
            try {
                Class<?> cls = Class.forName("org.ofbiz.order.shoppinglist.ShoppingListEvents");
                java.lang.reflect.Method m = cls.getMethod("addBulkFromCart", HttpServletRequest.class, HttpServletResponse.class);
                return (String) m.invoke(null, request, response);
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logError(e, "Error invoking ShoppingListEvents.addBulkFromCart via reflection", "PartymgrControllerDef");
                return "error";
            }
        }

        @Request(
            uri = "addListToCart",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String addListToCart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addListToCart
            try {
                Class<?> cls = Class.forName("org.ofbiz.order.shoppinglist.ShoppingListEvents");
                java.lang.reflect.Method m = cls.getMethod("addListToCart", HttpServletRequest.class, HttpServletResponse.class);
                return (String) m.invoke(null, request, response);
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logError(e, "Error invoking ShoppingListEvents.addListToCart via reflection", "PartymgrControllerDef");
                return "error";
            }
        }

        @Request(
            uri = "updateShoppingListItem",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        @Event(type = "service", invoke = "updateShoppingListItem")
        public static String updateShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "removeFromShoppingList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        @Event(type = "service", invoke = "removeShoppingListItem")
        public static String removeFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "replaceShoppingListItem",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String replaceShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.replaceShoppingListItem
            try {
                Class<?> cls = Class.forName("org.ofbiz.order.shoppinglist.ShoppingListEvents");
                java.lang.reflect.Method m = cls.getMethod("replaceShoppingListItem", HttpServletRequest.class, HttpServletResponse.class);
                return (String) m.invoke(null, request, response);
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logError(e, "Error invoking ShoppingListEvents.replaceShoppingListItem via reflection", "PartymgrControllerDef");
                return "error";
            }
        }

        @Request(
            uri = "restoreCartFromList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        public static String restoreCartFromList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.restoreAutoSaveList
            try {
                Class<?> cls = Class.forName("org.ofbiz.order.shoppinglist.ShoppingListEvents");
                java.lang.reflect.Method m = cls.getMethod("restoreAutoSaveList", HttpServletRequest.class, HttpServletResponse.class);
                return (String) m.invoke(null, request, response);
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logError(e, "Error invoking ShoppingListEvents.restoreAutoSaveList via reflection", "PartymgrControllerDef");
                return "error";
            }
        }

        @Request(
            uri = "editcreditcard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcreditcard")
        public interface Editcreditcard {}

        @Request(
            uri = "createCreditCard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "createCreditCard")
        public static String createCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCreditCard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcreditcard")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "updateCreditCard")
        public static String updateCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editgiftcard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editgiftcard")
        public interface Editgiftcard {}

        @Request(
            uri = "createGiftCard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "editgiftcard")
        @Event(type = "service", invoke = "createGiftCard")
        public static String createGiftCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGiftCard",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editgiftcard")
        @Response(name = "error", type = "view", value = "editgiftcard")
        @Event(type = "service", invoke = "updateGiftCard")
        public static String updateGiftCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editeftaccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editeftaccount")
        public interface Editeftaccount {}

        @Request(
            uri = "createEftAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editeftaccount")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editeftaccount")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "createEftAccount")
        public static String createEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEftAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editeftaccount")
        @Response(name = "error", type = "view", value = "editeftaccount")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "updateEftAccount")
        public static String updateEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAvsOverride",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml", invoke = "updateAVSOverride")
        public static String updateAvsOverride(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "resetAvsOverride",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml", invoke = "deleteAVSOverride")
        public static String resetAvsOverride(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePaymentMethod",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "deletePaymentMethod")
        public static String deletePaymentMethod(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createnew",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateNewParty")
        public interface Createnew {}

        @Request(
            uri = "NewCustomer",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewCustomer")
        public interface NewCustomer {}

        @Request(
            uri = "createCustomer",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "NewCustomer")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/user/UserEvents.xml", invoke = "createCustomer")
        public static String createCustomer(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "NewProspect",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewProspect")
        public interface NewProspect {}

        @Request(
            uri = "createProspect",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "NewProspect")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/user/UserEvents.xml", invoke = "createProspect")
        public static String createProspect(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "NewEmployee",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewEmployee")
        public interface NewEmployee {}

        @Request(
            uri = "createEmployee",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "viewprofile")
        @Response(name = "error", type = "view", value = "NewEmployee")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/user/UserEvents.xml", invoke = "createEmployee")
        public static String createEmployee(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editperson",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerson")
        public interface Editperson {}

        @Request(
            uri = "createPerson",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditPerson")
        @Event(type = "service", invoke = "createPerson")
        public static String createPerson(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePerson",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view", value = "EditPerson")
        @Event(type = "service", invoke = "updatePerson")
        public static String updatePerson(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editpartygroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyGroup")
        public interface Editpartygroup {}

        @Request(
            uri = "createPartyGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditPartyGroup")
        @Event(type = "service", invoke = "createPartyGroup")
        public static String createPartyGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditPartyGroup")
        @Event(type = "service", invoke = "updatePartyGroup")
        public static String updatePartyGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewvendor",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewvendor")
        public interface Viewvendor {}

        @Request(
            uri = "createVendor",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewvendor")
        @Response(name = "error", type = "view", value = "viewvendor")
        @Event(type = "service", invoke = "createVendor")
        public static String createVendor(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateVendor",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewvendor")
        @Response(name = "error", type = "view", value = "viewvendor")
        @Event(type = "service", invoke = "updateVendor")
        public static String updateVendor(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editPartyAttribute",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyAttribute")
        public interface EditPartyAttribute {}

        @Request(
            uri = "createPartyAttribute",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditPartyAttribute")
        @Event(type = "service", invoke = "createPartyAttribute")
        public static String createPartyAttribute(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyAttribute",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditPartyAttribute")
        @Event(type = "service", invoke = "updatePartyAttribute")
        public static String updatePartyAttribute(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePartyAttribute",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "removePartyAttribute")
        public static String removePartyAttribute(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPartyTaxAuthInfos",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyTaxAuthInfos")
        public interface EditPartyTaxAuthInfos {}

        @Request(
            uri = "createPartyTaxAuthInfo",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyTaxAuthInfos")
        @Response(name = "error", type = "view", value = "EditPartyTaxAuthInfos")
        @Event(type = "service", invoke = "createPartyTaxAuthInfo")
        public static String createPartyTaxAuthInfo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyTaxAuthInfo",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyTaxAuthInfos")
        @Response(name = "error", type = "view", value = "EditPartyTaxAuthInfos")
        @Event(type = "service", invoke = "updatePartyTaxAuthInfo")
        public static String updatePartyTaxAuthInfo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyTaxAuthInfo",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyTaxAuthInfos")
        @Response(name = "error", type = "view", value = "EditPartyTaxAuthInfos")
        @Event(type = "service", invoke = "deletePartyTaxAuthInfo")
        public static String deletePartyTaxAuthInfo(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findparty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findparty")
        public interface Findparty {}

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "AddPartyNote",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddPartyNote")
        public interface AddPartyNote {}

        @Request(
            uri = "createPartyNote",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "AddPartyNote")
        @Event(type = "service", invoke = "createPartyNote")
        public static String createPartyNote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCustRequest",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "viewprofile")
        @Response(name = "error", type = "request-redirect", value = "viewprofile")
        @Event(type = "service", invoke = "createCustRequest")
        public static String createCustRequest(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "newrequest",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        public interface Newrequest {}

        @Request(
            uri = "createrequest",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam", value = "RequestList")
        @Response(name = "error", type = "view", value = "EditRequest")
        @Event(type = "service", invoke = "createCustRequest")
        public static String createrequest(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListPartyContactLists",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPartyContactLists")
        public interface ListPartyContactLists {}

        @Request(
            uri = "createContactListParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPartyContactLists")
        @Response(name = "error", type = "view", value = "ListPartyContactLists")
        @Event(type = "service", invoke = "createContactListParty")
        public static String createContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactListParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPartyContactLists")
        @Response(name = "error", type = "view", value = "ListPartyContactLists")
        @Event(type = "service", invoke = "updateContactListParty")
        public static String updateContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPartyRates",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRates")
        public interface EditPartyRates {}

        @Request(
            uri = "createPartyRate",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRates")
        @Response(name = "error", type = "view", value = "EditPartyRates")
        @Event(type = "service", invoke = "updatePartyRate")
        public static String createPartyRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyRate",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRates")
        @Response(name = "error", type = "view", value = "EditPartyRates")
        @Event(type = "service", invoke = "updatePartyRate")
        public static String updatePartyRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyRate",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRates")
        @Response(name = "error", type = "view", value = "EditPartyRates")
        @Event(type = "service", invoke = "deletePartyRate")
        public static String deletePartyRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewSegmentRoles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSegmentRoles")
        public interface ViewSegmentRoles {}

        @Request(
            uri = "createSegmentRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSegmentRoles")
        @Response(name = "error", type = "view", value = "ViewSegmentRoles")
        @Event(type = "service", invoke = "createSegmentGroupRole")
        public static String createSegmentRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSegmentGroupRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSegmentRoles")
        @Response(name = "error", type = "view", value = "ViewSegmentRoles")
        @Event(type = "service", invoke = "deleteSegmentGroupRole")
        public static String deleteSegmentGroupRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addsecondaryroles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "addsecondaryroles")
        public interface Addsecondaryroles {}

        @Request(
            uri = "addrole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "createPartyRole")
        public static String addrole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleterole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewroles")
        @Response(name = "error", type = "view", value = "viewroles")
        @Event(type = "service", invoke = "deletePartyRole")
        public static String deleterole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createroletype",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewroles")
        @Response(name = "error", type = "view", value = "viewroles")
        @Event(type = "service", invoke = "createRoleType")
        public static String createroletype(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyIdentification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewidentifications")
        @Response(name = "error", type = "view", value = "viewidentifications")
        @Event(type = "service", invoke = "createPartyIdentification")
        public static String createPartyIdentification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "deletePartyIdentification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewidentifications")
        @Response(name = "error", type = "view", value = "viewidentifications")
        @Event(type = "service", invoke = "deletePartyIdentification")
        public static String deletePartyIdentification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyIdentification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewidentifications")
        @Response(name = "error", type = "view", value = "viewidentifications")
        @Event(type = "service", invoke = "updatePartyIdentification")
        public static String updatePartyIdentification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyRelationshipType",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRelationships")
        @Response(name = "error", type = "view", value = "EditPartyRelationships")
        @Event(type = "service", invoke = "createPartyRelationshipType")
        public static String createPartyRelationshipType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyRelationship",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRelationships")
        @Response(name = "error", type = "view", value = "EditPartyRelationships")
        @Event(type = "service", invoke = "createPartyRelationship")
        public static String createPartyRelationship(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyRelationshipAndRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "backHome")
        @Response(name = "error", type = "request-redirect-noparam", value = "backHome")
        @Event(type = "service", invoke = "createPartyRelationshipAndRole")
        public static String createPartyRelationshipAndRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyRelationshipContactAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home")
        @Response(name = "error", type = "view-home")
        @Event(type = "service", invoke = "createPartyRelationshipContactAccount")
        public static String createPartyRelationshipContactAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyRelationship",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRelationships")
        @Response(name = "error", type = "view", value = "EditPartyRelationships")
        @Event(type = "service", invoke = "updatePartyRelationship")
        public static String updatePartyRelationship(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyRelationship",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyRelationships")
        @Response(name = "error", type = "view", value = "EditPartyRelationships")
        @Event(type = "service", invoke = "deletePartyRelationship")
        public static String deletePartyRelationship(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findVisits",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findVisits")
        public interface FindVisits {}

        @Request(
            uri = "visitdetail",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "visitdetail")
        public interface Visitdetail {}

        @Request(
            uri = "listLoggedInUsers",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listLoggedInUsers")
        public interface ListLoggedInUsers {}

        @Request(
            uri = "pushPage",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "visitdetail")
        public static String pushPage(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.setFollowerPage
            return CommonEvents.setFollowerPage(request, response);
        }

        @Request(
            uri = "setAppletFollower",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "visitdetail")
        public static String setAppletFollower(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.setAppletFollower
            return CommonEvents.setAppletFollower(request, response);
        }

        @Request(
            uri = "setCommunicationEventRoleStatus",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "setCommunicationEventRoleStatus")
        public static String setCommunicationEventRoleStatus(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "MyCommunicationEvents",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "MyCommunicationEvents", saveHomeView = "true")
        public interface MyCommunicationEvents {}

        @Request(
            uri = "FindCommunicationEvents",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCommunicationEvents", saveHomeView = "true")
        public interface FindCommunicationEvents {}

        @Request(
            uri = "ListCommContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCommContent")
        public interface ListCommContent {}

        @Request(
            uri = "PendingCommunications",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PendingCommunications")
        public interface PendingCommunications {}

        @Request(
            uri = "EditCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommunicationEvent", saveCurrentView = "true")
        public interface EditCommunicationEvent {}

        @Request(
            uri = "ViewCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewCommunicationEvent")
        public interface ViewCommunicationEvent {}

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "uploadAttachFiletoEmail",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "EditCommunicationEvent")
        @Response(name = "error", type = "view-home", value = "EditCommunicationEvent")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/communication/CommunicationEventEvents.xml", invoke = "createCommunicationEventContent")
        public static String uploadAttachFiletoEmail(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadAttachFile",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCommContent")
        @Response(name = "error", type = "view", value = "ListCommContent")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/communication/CommunicationEventEvents.xml", invoke = "createCommunicationEventContent")
        public static String uploadAttachFile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeAttachFile",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditCommunicationEvent")
        @Response(name = "error", type = "view", value = "EditCommunicationEvent")
        @Event(type = "service", invoke = "removeCommEventContentAssoc")
        public static String removeAttachFile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddCommContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddCommContent")
        public interface AddCommContent {}

        @Request(
            uri = "EditCommContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommContent")
        public interface EditCommContent {}

        @Request(
            uri = "NewDraftCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommunicationEvent", saveCurrentView = "true")
        @Event(type = "service", invoke = "createCommunicationEvent")
        public static String newDraftCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view", value = "EditCommunicationEvent")
        @Event(type = "service", invoke = "createCommunicationEvent")
        public static String createCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam", value = "EditCommunicationEvent")
        @Response(name = "error", type = "view", value = "EditCommunicationEvent")
        @Event(type = "service", invoke = "updateCommunicationEvent")
        public static String updateCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "sendCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam", value = "EditCommunicationEvent")
        @Response(name = "error", type = "view", value = "EditCommunicationEvent")
        @Event(type = "service", invoke = "updateCommunicationEvent")
        public static String sendCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "deleteCommunicationEvent")
        public static String deleteCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteUnknownCommunicationEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last", saveLastView = "true")
        @Response(name = "error", type = "view-last", saveLastView = "true")
        @Event(type = "service", invoke = "deleteCommunicationEvent")
        public static String deleteUnknownCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommunicationEvents",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home")
        @Response(name = "error", type = "view-home")
        @Event(type = "service-multi", invoke = "deleteCommunicationEvent")
        public static String deleteCommunicationEvents(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listUnknownPartyComms",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListUnknownPartyComms", saveHomeView = "true")
        public interface ListUnknownPartyComms {}

        @Request(
            uri = "FindCommunicationByOrder",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCommunicationByOrder")
        public interface FindCommunicationByOrder {}

        @Request(
            uri = "editRequestFromCommEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestFromCommEvent", saveLastView = "true")
        public interface EditRequestFromCommEvent {}

        @Request(
            uri = "createRequestFromCommEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "createCustRequestFromCommEvent")
        public static String createRequestFromCommEvent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "allocateMsgToParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last-noparam")
        @Response(name = "error", type = "view-last", value = "ViewCommunicationEvent")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/communication/CommunicationEventEvents.xml", invoke = "allocateMsgToParty")
        public static String allocateMsgToParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCommContentDataResource",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "AddCommContent")
        @Response(name = "success", type = "view", value = "EditCommContent")
        @Event(type = "service", invoke = "createCommContentDataResource")
        public static String createCommContentDataResource(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCommContentDataResource",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommContent")
        @Response(name = "error", type = "view", value = "EditCommContent")
        @Event(type = "service", invoke = "updateCommContentDataResource")
        public static String updateCommContentDataResource(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadCommEventContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "persistContentAndAssoc")
        public static String uploadCommEventContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "UpdateCommPurposes",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommPurposes")
        public interface UpdateCommPurposes {}

        @Request(
            uri = "createCommunicationEventPurpose",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommPurposes")
        @Response(name = "error", type = "view", value = "UpdateCommPurposes")
        @Event(type = "service", invoke = "createCommunicationEventPurpose")
        public static String createCommunicationEventPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeCommunicationEventPurpose",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommPurposes")
        @Response(name = "error", type = "view", value = "UpdateCommPurposes")
        @Event(type = "service", invoke = "removeCommunicationEventPurpose")
        public static String removeCommunicationEventPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateCommRoles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommRoles", saveCurrentView = "true")
        public interface UpdateCommRoles {}

        @Request(
            uri = "createCommunicationEventRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "EditCommunicationEvent")
        @Response(name = "error", type = "view-home", value = "EditCommunicationEvent")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/communication/CommunicationEventEvents.xml", invoke = "createCommunicationEventRole")
        public static String createCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveCommunicationEventRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "removeCommunicationEventRole")
        public static String removeCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListPartyCommEvents",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPartyCommEvents")
        public interface ListPartyCommEvents {}

        @Request(
            uri = "showclassgroups",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyClassificationGroups")
        public interface Showclassgroups {}

        @Request(
            uri = "EditPartyClassifications",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassifications")
        public interface EditPartyClassifications {}

        @Request(
            uri = "EditPartyClassificationGroupParties",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroupParties")
        public interface EditPartyClassificationGroupParties {}

        @Request(
            uri = "EditPartyClassificationGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroup")
        public interface EditPartyClassificationGroup {}

        @Request(
            uri = "createPartyClassification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassifications")
        @Response(name = "error", type = "view", value = "EditPartyClassifications")
        @Event(type = "service", invoke = "createPartyClassification")
        public static String createPartyClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyClassification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassifications")
        @Response(name = "error", type = "view", value = "EditPartyClassifications")
        @Event(type = "service", invoke = "updatePartyClassification")
        public static String updatePartyClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyClassificationParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroupParties")
        @Response(name = "error", type = "view", value = "EditPartyClassificationGroupParties")
        @Event(type = "service", invoke = "createPartyClassification")
        public static String createPartyClassificationParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyClassificationParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroupParties")
        @Response(name = "error", type = "view", value = "EditPartyClassificationGroupParties")
        @Event(type = "service", invoke = "updatePartyClassification")
        public static String updatePartyClassificationParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyClassification",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassifications")
        @Response(name = "error", type = "view", value = "EditPartyClassifications")
        @Event(type = "service", invoke = "deletePartyClassification")
        public static String deletePartyClassification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyClassificationGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroup")
        @Response(name = "error", type = "view", value = "EditPartyClassificationGroup")
        @Event(type = "service", invoke = "createPartyClassificationGroup")
        public static String createPartyClassificationGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyClassificationGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyClassificationGroup")
        @Response(name = "error", type = "view", value = "EditPartyClassificationGroup")
        @Event(type = "service", invoke = "updatePartyClassificationGroup")
        public static String updatePartyClassificationGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyClassificationGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyClassificationGroups")
        @Response(name = "error", type = "view", value = "FindPartyClassificationGroups")
        @Event(type = "service", invoke = "deletePartyClassificationGroup")
        public static String deletePartyClassificationGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListCommWorkEfforts",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCommWorkEfforts")
        public interface ListCommWorkEfforts {}

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "AddCommEventWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddCommEventWorkEffort")
        public interface AddCommEventWorkEffort {}

        @Request(
            uri = "EditCommEventWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommEventWorkEffort")
        public interface EditCommEventWorkEffort {}

        @Request(
            uri = "createCommEventWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommEventWorkEffort")
        @Response(name = "error", type = "view", value = "AddCommEventWorkEffort")
        @Event(type = "service", invoke = "createCommEventWorkEffort")
        public static String createCommEventWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCommEventWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCommWorkEfforts")
        @Response(name = "error", type = "view", value = "ListCommWorkEfforts")
        @Event(type = "service", invoke = "updateCommunicationEventWorkEff")
        public static String updateCommEventWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommEventWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCommWorkEfforts")
        @Response(name = "error", type = "view", value = "ListCommWorkEfforts")
        @Event(type = "service", invoke = "deleteCommunicationEventWorkEff")
        public static String deleteCommEventWorkEffort(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ImportExport",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ImportExport")
        public interface ImportExport {}

        @Request(
            uri = "ExportPartyCsv.csv",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyExportCsv")
        public interface ExportPartyCsvCsv {}

        @Request(
            uri = "uploadParty",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ImportExport")
        @Response(name = "error", type = "view", value = "ImportExport")
        @Event(type = "service", invoke = "importParty")
        public static String uploadParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewSimpleContent",
            controller = "partymgr",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ViewSimpleContent")
        public interface ViewSimpleContent {}

        @Request(
            uri = "EditPartyContents",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyContents")
        public interface EditPartyContents {}

        @Request(
            uri = "createPartyContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyContents")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleEvents.xml", invoke = "createPartyContent")
        public static String createPartyContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyContents")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleEvents.xml", invoke = "updatePartyContent")
        public static String updatePartyContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePartyContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyContents")
        @Event(type = "service", invoke = "removePartyContent")
        public static String removePartyContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePartyContentAndRelated",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyContents")
        @Event(type = "service", invoke = "removePartyContentAndRelated")
        public static String removePartyContentAndRelated(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadPartyContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "partyContentList")
        @Response(name = "error", type = "view", value = "EventMessages")
        @Event(type = "service", invoke = "uploadPartyContentFile")
        public static String uploadPartyContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "partyContentList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "partyContentList")
        public interface PartyContentList {}

        @Request(
            uri = "img",
            controller = "partymgr"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "request", value = "main")
        public static String img(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.content.data.DataEvents.serveImage
            return DataEvents.serveImage(request, response);
        }

        @Request(
            uri = "editCarrierAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editCarrierAccount")
        public interface EditCarrierAccount {}

        @Request(
            uri = "createPartyCarrierAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "editCarrierAccount")
        @Event(type = "service", invoke = "createPartyCarrierAccount")
        public static String createPartyCarrierAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyCarrierAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "updatePartyCarrierAccount")
        public static String updatePartyCarrierAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "EditPartySkills",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        public interface EditPartySkills {}

        @Request(
            uri = "createPartySkillExt",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        @Response(name = "error", type = "view", value = "EditPartySkills")
        @Event(type = "service", invoke = "createPartySkill")
        public static String createPartySkillExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartySkillExt",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        @Response(name = "error", type = "view", value = "EditPartySkills")
        @Event(type = "service", invoke = "updatePartySkill")
        public static String updatePartySkillExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartySkill",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        @Event(type = "service", invoke = "deletePartySkill")
        public static String deletePartySkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPartyResumes",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        public interface EditPartyResumes {}

        @Request(
            uri = "createPartyResume",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        @Event(type = "service", invoke = "createPartyResume")
        public static String createPartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyResume",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        @Event(type = "service", invoke = "updatePartyResume")
        public static String updatePartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyResume",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        @Event(type = "service", invoke = "deletePartyResume")
        public static String deletePartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmploymentApps",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApps")
        public interface EditEmploymentApps {}

        @Request(
            uri = "createEmploymentAppExt",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditEmploymentApps")
        @Event(type = "service", invoke = "createEmploymentApp")
        public static String createEmploymentAppExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmploymentAppExt",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApps")
        @Event(type = "service-multi", invoke = "updateEmploymentApp")
        public static String updateEmploymentAppExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmploymentApp",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditEmploymentApps")
        @Event(type = "service", invoke = "deleteEmploymentApp")
        public static String deleteEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ajaxUpdatePartyGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "updatePartyGroup")
        public static String ajaxUpdatePartyGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditBillingAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccount")
        public interface EditBillingAccount {}

        @Request(
            uri = "createBillingAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditBillingAccount")
        @Event(type = "service", invoke = "createBillingAccount")
        public static String createBillingAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBillingAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccount")
        @Response(name = "error", type = "view", value = "EditBillingAccount")
        @Event(type = "service", invoke = "updateBillingAccount")
        public static String updateBillingAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteBillingAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml", invoke = "deleteBillingAccount")
        public static String deleteBillingAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateCommOrders",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommOrders")
        public interface UpdateCommOrders {}

        @Request(
            uri = "createCommunicationEventOrder",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommOrders")
        @Response(name = "error", type = "view", value = "UpdateCommOrders")
        @Event(type = "service", invoke = "createCommunicationEventOrder")
        public static String createCommunicationEventOrder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommunicationEventOrder",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommOrders")
        @Response(name = "error", type = "view", value = "UpdateCommOrders")
        @Event(type = "service", invoke = "removeCommunicationEventOrder")
        public static String deleteCommunicationEventOrder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "UpdateCommProducts",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommProducts")
        public interface UpdateCommProducts {}

        @Request(
            uri = "createCommunicationEventProduct",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommProducts")
        @Response(name = "error", type = "view", value = "UpdateCommProducts")
        @Event(type = "service", invoke = "createCommunicationEventProduct")
        public static String createCommunicationEventProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCommunicationEventProduct",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCommProducts")
        @Response(name = "error", type = "view", value = "UpdateCommProducts")
        @Event(type = "service", invoke = "removeCommunicationEventProduct")
        public static String deleteCommunicationEventProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupPartyName",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupPartyEmail",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyEmail")
        public interface LookupPartyEmail {}

        @Request(
            uri = "LookupPerson",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupContact",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContact")
        public interface LookupContact {}

        @Request(
            uri = "LookupLead",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupLead")
        public interface LookupLead {}

        @Request(
            uri = "LookupPartyAndUserLoginAndPerson",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyAndUserLoginAndPerson")
        public interface LookupPartyAndUserLoginAndPerson {}

        @Request(
            uri = "LookupUserLoginAndPartyDetails",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUserLoginAndPartyDetails")
        public interface LookupUserLoginAndPartyDetails {}

        @Request(
            uri = "LookupPartyGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyGroup")
        public interface LookupPartyGroup {}

        @Request(
            uri = "LookupAccount",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAccount")
        public interface LookupAccount {}

        @Request(
            uri = "LookupPartyClassificationGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyClassificationGroup")
        public interface LookupPartyClassificationGroup {}

        @Request(
            uri = "LookupCommEvent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCommEvent")
        public interface LookupCommEvent {}

        @Request(
            uri = "LookupCustRequest",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustRequest")
        public interface LookupCustRequest {}

        @Request(
            uri = "LookupSegmentGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSegmentGroup")
        public interface LookupSegmentGroup {}

        @Request(
            uri = "LookupContactList",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactList")
        public interface LookupContactList {}

        @Request(
            uri = "LookupWorkEffort",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "LookupContent",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}

        @Request(
            uri = "LookupDataResource",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupDataResource")
        public interface LookupDataResource {}

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "LookupPreferredContactMech",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPreferredContactMech")
        public interface LookupPreferredContactMech {}

        @Request(
            uri = "LookupEmploymentApp",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupEmploymentApp")
        public interface LookupEmploymentApp {}

        @Request(
            uri = "LookupEmplPosition",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupEmplPosition")
        public interface LookupEmplPosition {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

        @Request(
            uri = "LookupProduct",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "PartyFinancialHistory",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyFinancialHistory")
        @Response(name = "error", type = "view", value = "viewprofile")
        public interface PartyFinancialHistory {}

        @Request(
            uri = "Preferences",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Preferences")
        public interface Preferences {}

        @Request(
            uri = "removePreference",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Preferences")
        @Response(name = "error", type = "view", value = "Preferences")
        @Event(type = "service", invoke = "removeUserPreference")
        public static String removePreference(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PartyGeoLocation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyGeoLocation")
        @Response(name = "error", type = "view", value = "viewprofile")
        public interface PartyGeoLocation {}

        @Request(
            uri = "GetPartyGeoLocation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GetPartyGeoLocation")
        @Response(name = "error", type = "view", value = "viewprofile")
        public interface GetPartyGeoLocation {}

        @Request(
            uri = "addGeoLocation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "addGeoLocation")
        @Response(name = "error", type = "view", value = "PartyGeoLocation")
        public interface AddGeoLocation {}

        @Request(
            uri = "editGeoLocation",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyGeoLocation")
        @Response(name = "error", type = "view", value = "PartyGeoLocation")
        @Event(type = "simple", path = "component://party/script/org/ofbiz/party/party/PartySimpleEvents.xml", invoke = "editGeoLocation")
        public static String editGeoLocation(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewProductStoreRoles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreRoles")
        public interface ViewProductStoreRoles {}

        @Request(
            uri = "FindProductStoreRoles",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreRoles")
        public interface FindProductStoreRoles {}

        @Request(
            uri = "storeCreateRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreRoles")
        @Response(name = "error", type = "view", value = "ViewProductStoreRoles")
        @Event(type = "service", invoke = "createProductStoreRole")
        public static String storeCreateRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeUpdateRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreRoles")
        @Response(name = "error", type = "view", value = "ViewProductStoreRoles")
        @Event(type = "service", invoke = "updateProductStoreRole")
        public static String storeUpdateRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeRemoveRole",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreRoles")
        @Response(name = "error", type = "view", value = "ViewProductStoreRoles")
        @Event(type = "service", invoke = "removeProductStoreRole")
        public static String storeRemoveRole(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ceimages",
            controller = "partymgr",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        public static String ceimages(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.party.communication.CommunicationEventServices.markCommunicationAsRead
            return CommunicationEventServices.markCommunicationAsRead(request, response);
        }

        @Request(
            uri = "ProfileCreateNewLogin",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileCreateNewLogin")
        public interface ProfileCreateNewLogin {}

        @Request(
            uri = "ProfileCreateUserLogin",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLogin")
        @Response(name = "error", type = "view", value = "ProfileCreateNewLogin")
        @Event(type = "service", invoke = "createUserLogin")
        public static String profileCreateUserLogin(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "ProfileEditUserLogin",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLogin")
        public interface ProfileEditUserLogin {}

        @Request(
            uri = "ProfileUpdatePassword",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLogin")
        @Response(name = "error", type = "view", value = "ProfileEditUserLogin")
        @Event(type = "simple", path = "component://securityext/script/org/ofbiz/securityext/login/LoginSimpleEvents.xml", invoke = "updatePassword")
        public static String profileUpdatePassword(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProfileUpdateUserLoginSecurity",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLogin")
        @Response(name = "error", type = "view", value = "ProfileEditUserLogin")
        @Event(type = "service", invoke = "updateUserLoginSecurity")
        public static String profileUpdateUserLoginSecurity(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProfileAddUserLoginToSecurityGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Response(name = "error", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Event(type = "service", invoke = "addUserLoginToSecurityGroup")
        public static String profileAddUserLoginToSecurityGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProfileRemoveUserLoginFromSecurityGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Response(name = "error", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Event(type = "service", invoke = "removeUserLoginToSecurityGroup")
        public static String profileRemoveUserLoginFromSecurityGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProfileUpdateUserLoginToSecurityGroup",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Response(name = "error", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        @Event(type = "service", invoke = "updateUserLoginToSecurityGroup")
        public static String profileUpdateUserLoginToSecurityGroup(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProfileEditUserLoginSecurityGroups",
            controller = "partymgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProfileEditUserLoginSecurityGroups")
        public interface ProfileEditUserLoginSecurityGroups {}


    }
}
