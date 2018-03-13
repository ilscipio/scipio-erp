import java.sql.Timestamp

import org.ofbiz.base.crypto.HashCrypt
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.common.login.LoginServices
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript


public class PartyData extends DataGeneratorGroovyBaseScript {
	
	final String DATA_TYPE = "party";
    
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
		return DATA_TYPE;
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
        }
        return toBeStored;
    }
}