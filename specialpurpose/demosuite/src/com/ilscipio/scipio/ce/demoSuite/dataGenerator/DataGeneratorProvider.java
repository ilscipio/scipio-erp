package com.ilscipio.scipio.ce.demoSuite.dataGenerator;

import static java.lang.annotation.ElementType.TYPE;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders;

@Target(TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface DataGeneratorProvider {
    
    DataGeneratorProviders[] providers();
    
}
