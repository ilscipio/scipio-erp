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
package com.ilscipio.scipio.marketing.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Secas {

    /**
     * SECA for service createContactListPartyStatus on event commit.
     */
    @Seca(
        service = "createContactListPartyStatus",
        event = "commit",
        condition = "statusId == 'CLPT_PENDING'",
        actions = {
            @SecaAction(
                service = "sendContactListPartyVerifyEmail",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CreateContactListPartyStatuscommitSeca1 {}

    /**
     * SECA for service createContactListPartyStatus on event commit.
     */
    @Seca(
        service = "createContactListPartyStatus",
        event = "commit",
        condition = "statusId == 'CLPT_ACCEPTED'",
        actions = {
            @SecaAction(
                service = "sendContactListPartySubscribeEmail",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CreateContactListPartyStatuscommitSeca2 {}

    /**
     * SECA for service updatePartyEmailAddress on event return.
     */
    @Seca(
        service = "updatePartyEmailAddress",
        event = "return",
        actions = {
            @SecaAction(
                service = "updatePartyEmailContactListParty",
                mode = "sync"
            )
        }
    )
    public interface UpdatePartyEmailAddressreturnSeca3 {}

    /**
     * SECA for service updateContactListParty on event commit.
     */
    @Seca(
        service = "updateContactListParty",
        event = "commit",
        condition = "statusId == 'CLPT_REJECTED'",
        actions = {
            @SecaAction(
                service = "sendContactListPartyUnSubscribeEmail",
                mode = "sync",
                persist = "true"
            )
        }
    )
    public interface UpdateContactListPartycommitSeca4 {}

    /**
     * SECA for service createContactListPartyStatus on event commit.
     */
    @Seca(
        service = "createContactListPartyStatus",
        event = "commit",
        condition = "statusId == 'CLPT_UNSUBSCRIBED'",
        actions = {
            @SecaAction(
                service = "sendContactListPartyUnSubscribeEmail",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CreateContactListPartyStatuscommitSeca5 {}

    /**
     * SECA for service createContactListPartyStatus on event commit.
     */
    @Seca(
        service = "createContactListPartyStatus",
        event = "commit",
        condition = "statusId == 'CLPT_UNSUBS_PENDING'",
        actions = {
            @SecaAction(
                service = "sendContactListPartyUnSubscribeVerifyEmail",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CreateContactListPartyStatuscommitSeca6 {}

    /**
     * SECA for service updateCommunicationEvent on event commit.
     */
    @Seca(
        service = "updateCommunicationEvent",
        event = "commit",
        condition = "statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "updateCommStatusFromCommEvent",
                mode = "sync"
            )
        }
    )
    public interface UpdateCommunicationEventcommitSeca7 {}

    /**
     * SECA for service updateContactListCommStatus on event commit.
     */
    @Seca(
        service = "updateContactListCommStatus",
        event = "commit",
        condition = "statusId == 'COM_BOUNCED' && !empty(contactMechId) && !empty(partyId)",
        assignments = {
            @SecaSet(fieldName = "statusId", value = "CLPT_INVALID")
        },
        actions = {
            @SecaAction(
                service = "updateContactListParty",
                mode = "sync"
            )
        }
    )
    public interface UpdateContactListCommStatuscommitSeca8 {}

    /**
     * SECA for service createLead on event commit.
     */
    @Seca(
        service = "createLead",
        event = "commit",
        condition = "!empty(contactListId) && !empty(contactMechId)",
        assignments = {
            @SecaSet(fieldName = "statusId", value = "CLPT_ACCEPTED"),
            @SecaSet(fieldName = "preferredContactMechId", envName = "contactMechId")
        },
        actions = {
            @SecaAction(
                service = "createContactListParty",
                mode = "sync"
            )
        }
    )
    public interface CreateLeadcommitSeca9 {}

    /**
     * SECA for service createContact on event commit.
     */
    @Seca(
        service = "createContact",
        event = "commit",
        condition = "!empty(contactListId) && !empty(contactMechId)",
        assignments = {
            @SecaSet(fieldName = "statusId", value = "CLPT_ACCEPTED"),
            @SecaSet(fieldName = "preferredContactMechId", envName = "contactMechId")
        },
        actions = {
            @SecaAction(
                service = "createContactListParty",
                mode = "sync"
            )
        }
    )
    public interface CreateContactcommitSeca10 {}

}
