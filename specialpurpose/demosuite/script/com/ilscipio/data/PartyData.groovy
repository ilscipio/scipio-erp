import java.sql.Timestamp

import org.ofbiz.base.crypto.HashCrypt
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.common.login.LoginServices
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DemoSuiteDataWorker
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.MockarooDataGenerator
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript


public class PartyData extends DataGeneratorGroovyBaseScript {
    
    LinkedList prepareData() {
        final String resource_error = "DemoSuiteUiLabels";
        
        final String DEFAULT_USER_LOGIN_PWD = "ofbiz";
        
        // PartyTypeIds
        List<String> partyTypeIds = [
            "PARTY_GROUP",
            "PERSON"
        ]
        
        // PartyStatus
        List<String> partyStatus = [
            "PARTY_DISABLED",
            "PARTY_ENABLED"
        ]
         
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - PARTY DATA-=-=-=-", "");
        Map result = ServiceUtil.returnSuccess();
        
        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        List<GenericValue> partyEntrys = new ArrayList<GenericValue>();
         
        List<GenericValue> roleTypeIds = from("RoleType").cache(false).queryList();
        
        List<DemoDataPerson> generatedPersons = DemoSuiteDataWorker.generatePerson(num, MockarooDataGenerator.class);
        List<DemoDataUserLogin> generatedUserLogins = DemoSuiteDataWorker.generateUserLogin(num, MockarooDataGenerator.class);         
        if ( (UtilValidate.isNotEmpty(generatedPersons) && generatedPersons.size() == numRecords) 
            && (UtilValidate.isNotEmpty(generatedUserLogins) && generatedUserLogins.size() == numRecords)) {
            for (int i = 0; i < numRecords; i++) {
                String partyId = "GEN_" + delegator.getNextSeqId("demo-partyId");
                String partyTypeId = partyTypeIds.get(UtilRandom.random(partyTypeIds));        
                String partyStatusId = partyStatus.get(UtilRandom.random(partyStatus)); 
                minDate = UtilDateTime.nowDate();
                if (context.minDate != null)
                    minDate = new Date(context.minDate.getTime());
                Timestamp createdDate = Timestamp.valueOf(UtilRandom.generateRandomDate(minDate, context));
    //            Debug.log("partyId ====> " + partyId + " partyTypeId ======> " + partyTypeId + " partyStatusId ==========> " + partyStatusId);        
                Map fields = UtilMisc.toMap("partyId", partyId, "partyTypeId", partyTypeId, "statusId", partyStatusId, "description", partyId + " description", "createdDate", createdDate);    
                GenericValue party = delegator.makeValue("Party", fields);
                toBeStored.add(party);
    
                
                GenericValue roleType = roleTypeIds.get(UtilRandom.random(roleTypeIds));
                fields = UtilMisc.toMap("roleTypeId", roleType.roleTypeId, "partyId", partyId);        
                GenericValue partyRole = delegator.makeValue("PartyRole", fields);        
                toBeStored.add(partyRole);
                
                DemoDataPerson demoDataPerson = generatedPersons.get(i);            
                String salutation = demoDataPerson.getTitle();
                String firstName = demoDataPerson.getFirstName();
                String lastName = demoDataPerson.getLastName();
                String gender = demoDataPerson.getGender();
                
                g = "M";
                if (gender.toUpperCase().startsWith("F"))
                    g = "F";
                fields = UtilMisc.toMap("partyId", partyId, "salutation", salutation, "firstName", firstName, "lastName", lastName, "gender", g);
    //            Debug.log("partyId ========> " + partyId + "  salutation ==========> " + salutation + "    firstName ===============> " + firstName + "          lastName ===============> " + lastName + "   gender ===========> " + g);
                GenericValue person = delegator.makeValue("Person", fields);
                toBeStored.add(person);
                
                DemoDataUserLogin demoDataUserLogin = generatedUserLogins.get(0);
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
    //            Debug.log("partyId ========> " + partyId + "  userLoginId ==========> " + userLoginId);
                GenericValue userLogin = delegator.makeValue("UserLogin", fields);
                toBeStored.add(userLogin);
            }
        }
        
        return toBeStored;
    }
}