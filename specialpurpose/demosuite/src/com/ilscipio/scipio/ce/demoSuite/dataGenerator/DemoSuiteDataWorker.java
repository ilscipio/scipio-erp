package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataPerson;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataUserLogin;

public class DemoSuiteDataWorker {

    @SuppressWarnings("unchecked")
    public static List<DemoDataAddress> generateAddress(int count, Class<? extends ThirdPartyDataGenerator<? extends DemoDataObject>> dataGeneratorClass) {
        try {
            ThirdPartyDataGenerator<?> generator = (ThirdPartyDataGenerator<?>) dataGeneratorClass.getConstructor(Class.class)
                    .newInstance(DemoDataAddress.class);
            List<?> data = (List<?>) generator.retrieveData(count);
            return (List<DemoDataAddress>) data;
        } catch (IllegalAccessException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (IllegalArgumentException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (NoSuchMethodException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (SecurityException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (InvocationTargetException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), "");
        }

        return null;

    }

    @SuppressWarnings("unchecked")
    public static List<DemoDataProduct> generateProduct(int count, Class<? extends ThirdPartyDataGenerator<DemoDataProduct>> dataGeneratorClass) {
        try {
            ThirdPartyDataGenerator<?> generator = (ThirdPartyDataGenerator<?>) dataGeneratorClass.getConstructor(Class.class)
                    .newInstance(DemoDataProduct.class);
            List<?> data = (List<?>) generator.retrieveData(count);
            return (List<DemoDataProduct>) data;
        } catch (IllegalAccessException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (IllegalArgumentException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (NoSuchMethodException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (SecurityException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (InvocationTargetException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), "");
        }

        return null;

    }
    
    @SuppressWarnings("unchecked")
    public static List<DemoDataPerson> generatePerson(int count, Class<? extends ThirdPartyDataGenerator<DemoDataPerson>> dataGeneratorClass) {
        try {
            ThirdPartyDataGenerator<?> generator = (ThirdPartyDataGenerator<?>) dataGeneratorClass.getConstructor(Class.class)
                    .newInstance(DemoDataPerson.class);
            List<?> data = (List<?>) generator.retrieveData(count);
            return (List<DemoDataPerson>) data;
        } catch (IllegalAccessException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (IllegalArgumentException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (NoSuchMethodException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (SecurityException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (InvocationTargetException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), "");
        }

        return null;

    }
    
    @SuppressWarnings("unchecked")
    public static List<DemoDataUserLogin> generateUserLogin(int count, Class<? extends ThirdPartyDataGenerator<DemoDataUserLogin>> dataGeneratorClass) {
        try {
            ThirdPartyDataGenerator<?> generator = (ThirdPartyDataGenerator<?>) dataGeneratorClass.getConstructor(Class.class)
                    .newInstance(DemoDataUserLogin.class);
            List<?> data = (List<?>) generator.retrieveData(count);
            return (List<DemoDataUserLogin>) data;
        } catch (IllegalAccessException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (IllegalArgumentException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (NoSuchMethodException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (SecurityException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (InvocationTargetException e) {
            Debug.logError(e, e.getMessage(), "");
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), "");
        }

        return null;

    }

}
