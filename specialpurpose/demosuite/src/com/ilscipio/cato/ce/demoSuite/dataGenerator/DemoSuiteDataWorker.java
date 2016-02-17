package com.ilscipio.cato.ce.demoSuite.dataGenerator;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.ofbiz.base.util.Debug;

import com.ilscipio.cato.ce.demoSuite.dataGenerator.dataObject.DemoDataAddress;
import com.ilscipio.cato.ce.demoSuite.dataGenerator.dataObject.DemoDataObject;
import com.ilscipio.cato.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;

public class DemoSuiteDataWorker {

    @SuppressWarnings("unchecked")
    public static List<DemoDataAddress> generateAddress(int count, Class<? extends ThirdPartyDataGenerator<? extends DemoDataObject>> dataGeneratorClass) {
        try {
            ThirdPartyDataGenerator<?> generator = (ThirdPartyDataGenerator<?>) dataGeneratorClass.getConstructor(Class.class)
                    .newInstance(DemoDataAddress.class);
            List<?> data = (List<?>) generator.retrieveData(count);
//            Debug.log("retreived addresses ==============> " + data);
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
//            Debug.log("retreived products ==============> " + data);
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

}
