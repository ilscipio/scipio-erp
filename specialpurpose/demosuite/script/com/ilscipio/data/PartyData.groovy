import java.sql.Timestamp

import org.ofbiz.base.crypto.HashCrypt
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.common.login.LoginServices
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.DemoDataHelper.dataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

import javolution.util.FastList


public class PartyData extends DataGeneratorGroovyBaseScript {
    
    PartyData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - PARTY DATA-=-=-=-", "");
    }
    
    final String DEFAULT_USER_LOGIN_PWD = "scipio";
    
    // PartyTypeIds
    final List<String> partyTypeIds = [
        "PARTY_GROUP",
        "PERSON"
    ]
    
    // PartyStatus
    final List<String> partyStatus = [
        "PARTY_DISABLED",
        "PARTY_ENABLED"
    ]
    
	public String getDataType() {
		return dataTypeEnum.PARTY;
	}
	
	public void init() {				
        List<GenericValue> roleTypeIds = from("RoleType").cache(false).queryList();
        context.roleTypeIds = roleTypeIds;		
    }
    
    List prepareData(int index, DemoDataObject partyData) throws Exception {
        List<GenericValue> toBeStored = new ArrayList<GenericValue>();
        List<GenericValue> partyEntrys = new ArrayList<GenericValue>();		
		
		DataGenerator generator = context.generator;
        
        if (partyData) {
            String partyId = "GEN_" + delegator.getNextSeqId("demo-partyId");
            String partyTypeId = partyTypeIds.get(UtilRandom.random(partyTypeIds));        
            String partyStatusId = partyStatus.get(UtilRandom.random(partyStatus)); 
            
            Timestamp createdDate = UtilRandom.generateRandomTimestamp(context);
            Map fields = UtilMisc.toMap("partyId", partyId, "partyTypeId", partyTypeId, "statusId", partyStatusId, "description", partyId + " description", "createdDate", createdDate);    
            GenericValue party = delegator.makeValue("Party", fields);
            toBeStored.add(party);
            
            GenericValue roleType = context.roleTypeIds.get(UtilRandom.random(context.roleTypeIds));
            fields = UtilMisc.toMap("roleTypeId", roleType.roleTypeId, "partyId", partyId);        
            GenericValue partyRole = delegator.makeValue("PartyRole", fields);        
            toBeStored.add(partyRole);
            
            DemoDataPerson demoDataPerson = partyData.getPerson();            
            String salutation = demoDataPerson.getTitle();
            String firstName = demoDataPerson.getFirstName();
            String lastName = demoDataPerson.getLastName();
            String gender = demoDataPerson.getGender();
            
            g = "M";
            if (gender.toUpperCase().startsWith("F"))
                g = "F";
            fields = UtilMisc.toMap("partyId", partyId, "salutation", salutation, "firstName", firstName, "lastName", lastName, "gender", g);
            GenericValue person = delegator.makeValue("Person", fields);
            toBeStored.add(person);
            
			if (generator.getHelper().generateUserLogin()) {
	            DemoDataUserLogin demoDataUserLogin = partyData.getUserLogin();
	            String userLoginId = demoDataUserLogin.getUserLoginId();
	            String currentPassword = demoDataUserLogin.getCurrentPassword();
	            if (!context.generatePassword)
	                currentPassword = DEFAULT_USER_LOGIN_PWD;
	            boolean useEncryption = "true".equals(EntityUtilProperties.getPropertyValue("security.properties", "password.encrypt", delegator));
	            if (useEncryption)
	                currentPassword = HashCrypt.cryptUTF8(LoginServices.getHashType(), null, currentPassword)
	            userLoginEnabled = "Y";
	            if (partyStatusId.equals("PARTY_DISABLED"))
	                userLoginEnabled = "N";
	            
	            fields = UtilMisc.toMap("partyId", partyId, "userLoginId", userLoginId, "currentPassword", currentPassword, "enabled", userLoginEnabled);
	            GenericValue userLogin = delegator.makeValue("UserLogin", fields);
	            toBeStored.add(userLogin);
			}
			
			if (generator.getHelper().generateAddress()) {
				DemoDataAddress demoDataAddress = partyData.getAddress();
				
				GenericValue countryGeo = EntityUtil.getFirst(delegator.findByAnd("Geo", UtilMisc.toMap("geoCode", demoDataAddress.country, "geoTypeId", "COUNTRY"), UtilMisc.toList("geoId"), true));				
				if (countryGeo) {					
					String contactMechId = "GEN_" + delegator.getNextSeqId("demo-contactMechId");
					List<GenericValue> postalAddressToStore = FastList.newInstance();
					GenericValue contactMech = delegator.makeValue("ContactMech", ["contactMechTypeId" : "POSTAL_ADDRESS", "contactMechId" : contactMechId]);
					postalAddressToStore.add(contactMech);
					postalAddressToStore.add(delegator.makeValue("PostalAddress", 
						["contactMechId" : contactMech.contactMechId, "countryGeoId" : countryGeo.geoId, "address1" : demoDataAddress.street, "city" : demoDataAddress.city, "postalCode" : demoDataAddress.zip]));
					postalAddressToStore.add(delegator.makeValue("PartyContactMech", ["partyId" : partyId, "contactMechId" : contactMech.contactMechId, "fromDate" : UtilDateTime.nowTimestamp()]));
					postalAddressToStore.add(delegator.makeValue("PartyContactMechPurpose", ["partyId" : partyId, "contactMechId" : contactMech.contactMechId, "contactMechPurposeTypeId" : "GENERAL_LOCATION", "fromDate" : UtilDateTime.nowTimestamp()]));
					toBeStored.addAll(postalAddressToStore);
				}
			}
        }
        return toBeStored;
    }
}