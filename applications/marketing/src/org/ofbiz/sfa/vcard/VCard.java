/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/

package org.ofbiz.sfa.vcard;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.party.party.PartyHelper;
import org.ofbiz.party.party.PartyWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import ezvcard.Ezvcard;
import ezvcard.io.text.VCardReader;
import ezvcard.parameter.AddressType;
import ezvcard.parameter.EmailType;
import ezvcard.parameter.TelephoneType;
import ezvcard.property.Address;
import ezvcard.property.Email;
import ezvcard.property.FormattedName;
import ezvcard.property.StructuredName;
import ezvcard.property.Telephone;

public class VCard {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resourceError = "MarketingUiLabels";

    /**
     * import a vcard from byteBuffer. the reader use is ez-vcard, see official site https://github.com/mangstadt/ez-vcard/
     * @param dctx
     * @param context
     * @return
     * @throws IOException 
     */
    public static Map<String, Object> importVCard(DispatchContext dctx, Map<String, ? extends Object> context) throws IOException {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> result = ServiceUtil.returnSuccess();
        ByteBuffer byteBuffer = (ByteBuffer) context.get("infile");
        byte[] inputByteArray = byteBuffer.array();
        InputStream in = new ByteArrayInputStream(inputByteArray);
        Map<String, Object> serviceCtx = new HashMap<>();
        boolean isGroup = false;
        List<Map<String, String>> partiesCreated = new ArrayList<>();
        List<Map<String, String>> partiesExist = new ArrayList<>();
        @SuppressWarnings("unused")
        String partyName = ""; // TODO this is not used yet

        try {
            // SCIPIO: TODO: REMOVE: this is a check to make sure the PartyIdentificationType exists in DB so don't have to reseed
            // Eventually this will not be needed anymore...
            // <PartyIdentificationType description="VCard reference origin" partyIdentificationTypeId="VCARD_FN_ORIGIN" parentTypeId=""/>
            if (UtilValidate.isEmpty(EntityQuery.use(delegator).from("PartyIdentificationType").where("partyIdentificationTypeId", "VCARD_FN_ORIGIN").cache().queryOne())) {
                delegator.makeValue("PartyIdentificationType", "partyIdentificationTypeId", "VCARD_FN_ORIGIN", "description", "VCard reference origin").create();
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not ensure PartyIdentificationType VCARD_FN_ORIGIN exists in database", module);
        }

        try (VCardReader vCardReader = new VCardReader(in)) {
            ezvcard.VCard vcard = null;
            while ((vcard = vCardReader.readNext()) != null) {

                //Todo create a generic service to resolve duplicate party
                FormattedName formattedName = vcard.getFormattedName();
                if (formattedName != null) {
                    String refCardId = formattedName.getValue();
                    GenericValue partyIdentification = EntityQuery.use(delegator).from("PartyIdentification").where("partyIdentificationTypeId", "VCARD_FN_ORIGIN", "idValue", refCardId).queryFirst();
                    if (partyIdentification != null) {
                        partiesExist.add(UtilMisc.toMap("partyId", (String)partyIdentification.get("partyId")));
                        continue;
                    }
                    //TODO manage update
                }
                //check if it's already load
                isGroup = false;
                if (vcard.getKind() != null) isGroup = vcard.getKind().isGroup();

                StructuredName structuredName = vcard.getStructuredName();
                if (UtilValidate.isEmpty(structuredName)) continue;
                if (!isGroup) {
                    serviceCtx.put("firstName", structuredName.getGiven());
                    serviceCtx.put("lastName", structuredName.getFamily());
                    partyName = structuredName.getGiven() + " " + structuredName.getFamily();
                }

                // Resolve all postal Address
                for (Address address : vcard.getAddresses()) {
                    boolean workAddress = false;
                    for (AddressType addressType : address.getTypes()) {
                        if (AddressType.PREF.equals(addressType) || AddressType.WORK.equals(addressType)) {
                            workAddress = true;
                            break;
                        }
                    }
                    if (! workAddress) continue;

                    serviceCtx.put("address1", address.getStreetAddressFull());
                    serviceCtx.put("city", address.getLocality());
                    serviceCtx.put("postalCode", address.getPostalCode());
                    GenericValue countryGeo = EntityQuery.use(delegator).from("Geo")
                            .where(EntityCondition.makeCondition("geoTypeId", EntityOperator.EQUALS, "COUNTRY"),
                                    EntityCondition.makeCondition("geoName", EntityOperator.LIKE, address.getCountry()))
                            .cache().queryFirst();
                    if (countryGeo != null) {
                        serviceCtx.put("countryGeoId", countryGeo.get("geoId"));
                    }
                    GenericValue stateGeo = EntityQuery.use(delegator).from("Geo")
                            .where(EntityCondition.makeCondition("geoTypeId", EntityOperator.EQUALS, "STATE"),
                                    EntityCondition.makeCondition("geoName", EntityOperator.LIKE, address.getRegion()))
                            .cache().queryFirst();
                    if (stateGeo != null) {
                        serviceCtx.put("stateProvinceGeoId", stateGeo.get("geoId"));
                    }
                }

                int nbEmailAddr = (vcard.getEmails() != null) ? vcard.getEmails().size() : 0;
                for (Email email : vcard.getEmails()) {
                    if (nbEmailAddr > 1) {
                        boolean workEmail = false;
                        for (EmailType emailType : email.getTypes()) {
                            if (EmailType.PREF.equals(emailType) || EmailType.WORK.equals(emailType)) {
                                workEmail = true;
                                break;
                            }
                        }
                        if (! workEmail) continue;
                    }
                    String emailAddr = email.getValue();
                    if (UtilValidate.isEmail(emailAddr)) {
                        serviceCtx.put("emailAddress", emailAddr);
                    } else {
                        //TODO change uncorrect labellisation
                        String emailFormatErrMsg = UtilProperties.getMessage(resourceError, "SfaImportVCardEmailFormatError", locale);
                        vCardReader.close();
                        return ServiceUtil.returnError(UtilProperties.getMessage(resourceError, "MarketingEmailFormatError", UtilMisc.toMap("firstName", structuredName.getGiven(), "lastName", structuredName.getFamily(), "emailFOrmatErrMsg", emailFormatErrMsg), locale));

                    }
                }

                int nbPhone = (vcard.getTelephoneNumbers() != null) ? vcard.getTelephoneNumbers().size() : 0;
                for (Telephone phone : vcard.getTelephoneNumbers()) {
                    if (nbPhone > 1) {
                        boolean workPhone = false;
                        for (TelephoneType phoneType : phone.getTypes()) {
                            if (TelephoneType.PREF.equals(phoneType) || TelephoneType.WORK.equals(phoneType)) {
                                workPhone = true;
                                break;
                            }
                        }
                        if (! workPhone) continue;
                    }
                    String phoneAddr = phone.getText();
                    boolean internationalPhone = phoneAddr.startsWith("+") || phoneAddr.startsWith("00");
                    phoneAddr = StringUtil.removeNonNumeric(phoneAddr);
                    int indexLocal = 0;
                    if (internationalPhone) {
                        indexLocal = 4;
                        if (!phoneAddr.startsWith("00")) {
                            phoneAddr = phoneAddr.concat("00");
                        }
                        serviceCtx.put("areaCode", phoneAddr.substring(0, indexLocal));
                    }
                    serviceCtx.put("contactNumber", phoneAddr.substring(indexLocal));
                }

                /* TODO improve this part to manage party organization */

                GenericValue userLogin = (GenericValue) context.get("userLogin");
                serviceCtx.put("userLogin", userLogin);
                String serviceName = (String) context.get("serviceName");
                Map<String, Object> serviceContext = UtilGenerics.cast(context.get("serviceContext"));
                if (UtilValidate.isNotEmpty(serviceContext)) {
                    for (Map.Entry<String, Object> entry : serviceContext.entrySet()) {
                        serviceCtx.put(entry.getKey(), entry.getValue());
                    }
                }
                Map<String, Object> resp = dispatcher.runSync(serviceName, serviceCtx);
                if (ServiceUtil.isError(resp)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resp));
                }
                partiesCreated.add(UtilMisc.toMap("partyId", (String) resp.get("partyId")));

                if (formattedName != null) {
                    //store the origin creation
                    Map<String, Object> createPartyIdentificationMap = dctx.makeValidContext("createPartyIdentification", ModelService.IN_PARAM, context);
                    createPartyIdentificationMap.put("partyId", resp.get("partyId"));
                    createPartyIdentificationMap.put("partyIdentificationTypeId", "VCARD_FN_ORIGIN");
                    createPartyIdentificationMap.put("idValue", formattedName.getValue());
                    resp = dispatcher.runSync("createPartyIdentification", createPartyIdentificationMap);
                    if (ServiceUtil.isError(resp)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resp));
                    }
                }
            }
        } catch (IOException | GenericEntityException | GenericServiceException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceError,
                    "SfaImportVCardError", UtilMisc.toMap("errorString", e.getMessage()), locale));
        }
        result.put("partiesCreated", partiesCreated);
        result.put("partiesExist", partiesExist);
        
        // SCIPIO: 2018-09-12: backward-compat: We return the first party created for backward-compat,
        // otherwise existing one
        if (partiesCreated.size() > 0) {
            result.put("partyId", partiesCreated.get(0).get("partyId"));
        } else if (partiesExist.size() > 0) {
            result.put("partyId", partiesExist.get(0).get("partyId"));
        }
        // SCIPIO: log the results in case screen fails
        final int maxPartiesToList = 10;
        Debug.logInfo("importVCard: " + partiesCreated.size() + " parties created (" 
                + (partiesCreated.size() > maxPartiesToList ? partiesCreated.subList(0, maxPartiesToList) + "..." : partiesCreated) 
                + "), " + partiesExist.size() + " parties already existed (" 
                + (partiesExist.size() > maxPartiesToList ? partiesExist.subList(0, maxPartiesToList) + "..." : partiesExist)  + ")", module);

        return result;
    }

    public static Map<String, Object> exportVCard(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String partyId = (String) context.get("partyId");
        Locale locale = (Locale) context.get("locale");
        File file = null;
        try {
            ezvcard.VCard vcard = new ezvcard.VCard();
            StructuredName structuredName = new StructuredName();
            GenericValue person = EntityQuery.use(delegator).from("Person").where("partyId", partyId).queryOne();
            if (person != null) {
                if (UtilValidate.isNotEmpty(person.getString("firstName")))
                    structuredName.setGiven(person.getString("firstName"));
                if (UtilValidate.isNotEmpty(person.getString("lastName")))
                    structuredName.setFamily(person.getString("lastName"));
                vcard.setStructuredName(structuredName);
            }
            String fullName = PartyHelper.getPartyName(delegator, partyId, false);
            vcard.setFormattedName(fullName);

            GenericValue postalAddress = PartyWorker.findPartyLatestPostalAddress(partyId, delegator);
            if (postalAddress != null) {
                Address address =  new Address();
                address.setStreetAddress(postalAddress.getString("address1"));
                address.setLocality(postalAddress.getString("city"));
                address.setPostalCode(postalAddress.getString("postalCode"));
                GenericValue state = postalAddress.getRelatedOne("StateProvinceGeo", false);
                if (state != null) {
                    address.setRegion(state.getString("geoName"));
                }
                GenericValue countryGeo = postalAddress.getRelatedOne("CountryGeo", false);
                if (countryGeo != null) {
                    String country = postalAddress.getRelatedOne("CountryGeo", false).getString("geoName");
                    address.setCountry(country);
                    address.getTypes().add(AddressType.WORK);;
                    //TODO : this can be better set by checking contactMechPurposeTypeId
                }
                vcard.addAddress(address);
            }

            GenericValue telecomNumber = PartyWorker.findPartyLatestTelecomNumber(partyId, delegator);
            if (telecomNumber != null) {
                Telephone tel = new Telephone(telecomNumber.getString("areaCode") + telecomNumber.getString("contactNumber"));
                tel.getTypes().add(TelephoneType.WORK);
                vcard.addTelephoneNumber(tel);
                //TODO : this can be better set by checking contactMechPurposeTypeId
            }

            GenericValue emailAddress = PartyWorker.findPartyLatestContactMech(partyId, "EMAIL_ADDRESS", delegator);
            if (emailAddress != null && UtilValidate.isNotEmpty(emailAddress.getString("infoString"))) {
                vcard.addEmail(new Email(emailAddress.getString("infoString")));
            }

            //TODO : convert to directdownload of a vcf file
            String saveToDirectory = EntityUtilProperties.getPropertyValue("sfa", "save.outgoing.directory", "", delegator);
            if (UtilValidate.isEmpty(saveToDirectory)) {
                saveToDirectory = System.getProperty("ofbiz.home");
            }
            String saveToFilename = fullName + ".vcf";
            file = FileUtil.getFile(saveToDirectory + "/" + saveToFilename);
            Ezvcard.write(vcard).go(file);
        } catch (FileNotFoundException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceError, 
                    "SfaExportVCardErrorOpeningFile", UtilMisc.toMap("errorString", file.getAbsolutePath()), locale));
        } catch (IOException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceError, 
                    "SfaExportVCardErrorWritingFile", UtilMisc.toMap("errorString", file.getAbsolutePath()), locale));
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceError, 
                    "SfaExportVCardError", UtilMisc.toMap("errorString", e.getMessage()), locale));
        }
        return ServiceUtil.returnSuccess();
    }
}
