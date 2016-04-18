package com.ilscipio.cato.util;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

public class CatoAntRDBMSTask extends Task {

    private String input;

    @Override
    public void execute() throws BuildException {
        try {
//            Delegator delegator = DelegatorFactory.getDelegator("default");
//            DelegatorContainer.
          
            
        } catch (Exception e) {
            System.err.println(e);
        }
        // delegator.getModelGroupReader().
        // delegator.
        // ModelGroupReader.getModelGroupReader("default").
        // ModelReader.getModelReader("default").
        super.execute();
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

}
