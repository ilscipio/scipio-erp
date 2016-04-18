package com.ilscipio.cato.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

public class CatoAntHelpTask extends Task {

    private String input;

    @Override
    public void execute() throws BuildException {     
        StringBuilder builder = new StringBuilder();
        Matcher m = Pattern.compile("(^\\x20+.*$)", Pattern.MULTILINE).matcher(input);
        while (m.find()) {
            builder.append(m.group());
            builder.append(System.getProperty("line.separator"));            
        }
        getProject().setNewProperty("projectHelpOutputParsed", builder.toString());
        super.execute();
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

}
