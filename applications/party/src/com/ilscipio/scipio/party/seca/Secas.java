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
package com.ilscipio.scipio.party.seca;

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
     * SECA for service createPerson on event commit.
     */
    @Seca(
        service = "createPerson",
        event = "commit",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface CreatePersoncommitSeca1 {}

    /**
     * SECA for service createProductStoreRole on event invoke.
     */
    @Seca(
        service = "createProductStoreRole",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateProductStoreRoleinvokeSeca2 {}

    /**
     * SECA for service createPartyGroup on event commit.
     */
    @Seca(
        service = "createPartyGroup",
        event = "commit",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface CreatePartyGroupcommitSeca3 {}

    /**
     * SECA for service createAffiliate on event commit.
     */
    @Seca(
        service = "createAffiliate",
        event = "commit",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface CreateAffiliatecommitSeca4 {}

    /**
     * SECA for service updatePerson on event invoke.
     */
    @Seca(
        service = "updatePerson",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "savePartyNameChange",
                mode = "sync"
            )
        }
    )
    public interface UpdatePersoninvokeSeca5 {}

    /**
     * SECA for service updatePartyGroup on event invoke.
     */
    @Seca(
        service = "updatePartyGroup",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "savePartyNameChange",
                mode = "sync"
            )
        }
    )
    public interface UpdatePartyGroupinvokeSeca6 {}

    /**
     * SECA for service createPartyRelationship on event invoke.
     */
    @Seca(
        service = "createPartyRelationship",
        event = "invoke",
        condition = "roleTypeIdFrom == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensurePartyRoleFrom",
                mode = "sync"
            )
        }
    )
    public interface CreatePartyRelationshipinvokeSeca7 {}

    /**
     * SECA for service createPartyRelationship on event invoke.
     */
    @Seca(
        service = "createPartyRelationship",
        event = "invoke",
        condition = "roleTypeIdTo == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensurePartyRoleTo",
                mode = "sync"
            )
        }
    )
    public interface CreatePartyRelationshipinvokeSeca8 {}

    /**
     * SECA for service createPartyContactMech on event commit.
     */
    @Seca(
        service = "createPartyContactMech",
        event = "commit",
        condition = "!empty(contactMechPurposeTypeId) && !empty(contactMechId)",
        actions = {
            @SecaAction(
                service = "createPartyContactMechPurpose",
                mode = "sync"
            )
        }
    )
    public interface CreatePartyContactMechcommitSeca9 {}

    /**
     * SECA for service createCommEventWorkEffort on event invoke.
     */
    @Seca(
        service = "createCommEventWorkEffort",
        event = "invoke",
        condition = "empty(workEffortId)",
        actions = {
            @SecaAction(
                service = "createWorkEffort",
                mode = "sync"
            )
        }
    )
    public interface CreateCommEventWorkEffortinvokeSeca10 {}

    /**
     * SECA for service sendMailMultiPart on event in-validate.
     */
    @Seca(
        service = "sendMailMultiPart",
        event = "in-validate",
        condition = "!empty(partyId) && empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "createCommEventFromEmail",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface SendMailMultiPartinvalidateSeca11 {}

    /**
     * SECA for service sendMail on event in-validate.
     */
    @Seca(
        service = "sendMail",
        event = "in-validate",
        condition = "!empty(partyId) && empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "createCommEventFromEmail",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface SendMailinvalidateSeca12 {}

    /**
     * SECA for service sendMailMultiPart on event commit.
     */
    @Seca(
        service = "sendMailMultiPart",
        event = "commit",
        condition = "!empty(messageWrapper) && !empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "updateCommEventAfterEmail",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface SendMailMultiPartcommitSeca13 {}

    /**
     * SECA for service sendMail on event commit.
     */
    @Seca(
        service = "sendMail",
        event = "commit",
        condition = "!empty(messageWrapper) && !empty(communicationEventId)",
        actions = {
            @SecaAction(
                service = "updateCommEventAfterEmail",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface SendMailcommitSeca14 {}

    /**
     * SECA for service sendEmailToContactList on event commit.
     */
    @Seca(
        service = "sendEmailToContactList",
        event = "commit",
        actions = {
            @SecaAction(
                service = "setCommEventComplete",
                mode = "sync"
            )
        }
    )
    public interface SendEmailToContactListcommitSeca15 {}

    /**
     * SECA for service updatePassword on event commit.
     */
    @Seca(
        service = "updatePassword",
        event = "commit",
        actions = {
            @SecaAction(
                service = "sendUpdatePersonalInfoEmailNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface UpdatePasswordcommitSeca16 {}

    /**
     * SECA for service createEmailAddressVerification on event commit.
     */
    @Seca(
        service = "createEmailAddressVerification",
        event = "commit",
        condition = "sendVerificationEmail == 'true'",
        actions = {
            @SecaAction(
                service = "sendVerifyEmailAddressNotification",
                mode = "async"
            )
        }
    )
    public interface CreateEmailAddressVerificationcommitSeca17 {}

    /**
     * SECA for service updateCommunicationEvent on event commit.
     */
    @Seca(
        service = "updateCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'AUTO_EMAIL_COM' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface UpdateCommunicationEventcommitSeca18 {}

    /**
     * SECA for service updateCommunicationEvent on event commit.
     */
    @Seca(
        service = "updateCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'EMAIL_COMMUNICATION' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface UpdateCommunicationEventcommitSeca19 {}

    /**
     * SECA for service updateCommunicationEvent on event commit.
     */
    @Seca(
        service = "updateCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'COMMENT_NOTE' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface UpdateCommunicationEventcommitSeca20 {}

    /**
     * SECA for service createCommunicationEvent on event commit.
     */
    @Seca(
        service = "createCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'AUTO_EMAIL_COM' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface CreateCommunicationEventcommitSeca21 {}

    /**
     * SECA for service createCommunicationEvent on event commit.
     */
    @Seca(
        service = "createCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'EMAIL_COMMUNICATION' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface CreateCommunicationEventcommitSeca22 {}

    /**
     * SECA for service createCommunicationEvent on event commit.
     */
    @Seca(
        service = "createCommunicationEvent",
        event = "commit",
        condition = "statusId == 'COM_ENTERED' && communicationEventTypeId == 'COMMENT_NOTE' && !empty(partyIdTo)",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyIdTo"),
            @SecaSet(fieldName = "noteInfo", envName = "subject"),
            @SecaSet(fieldName = "moreInfoItemName", value = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "communicationEventId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/partymgr/control/MyCommunicationEvents")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface CreateCommunicationEventcommitSeca23 {}

    /**
     * SECA for service updatePartyPostalAddress on event commit.
     */
    @Seca(
        service = "updatePartyPostalAddress",
        event = "commit",
        condition = "updatePartyProfileIds != 'false' && contactMechId != oldContactMechId",
        actions = {
            @SecaAction(
                service = "updatePartyProfileDefaultPostalAddressIds",
                mode = "sync"
            )
        }
    )
    public interface UpdatePartyPostalAddresscommitSeca24 {}

}
