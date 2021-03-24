package com.ilscipio.scipio.ce.build.util;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class BuildUtil {

    public static int getJavaVersionMajor() {
        String version = System.getProperty("java.version");
        if (version.startsWith("1.")) {
            return Integer.parseInt(version.substring(2, 3));
        } else {
            int sep = version.indexOf(".");
            if (sep > 1) {
                return Integer.parseInt(version.substring(0, sep));
            } else {
                return Integer.parseInt(version);
            }
        }
    }

    public static List<String> getIvyConfNames(String ivyConfPath) throws IOException {
        File inputFile = new File(ivyConfPath);
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder;
        try {
            dBuilder = dbFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
        Document doc;
        try {
            doc = dBuilder.parse(inputFile);
        } catch (SAXException e) {
            throw new IOException(e);
        }
        List<? extends Element> configurationsElemList = childElementList(doc.getDocumentElement(), "configurations");
        if (configurationsElemList == null || configurationsElemList.isEmpty()) {
            return Collections.emptyList();
        }
        List<? extends Element> confElemList = childElementList(configurationsElemList.get(0), "conf");
        List<String> confList = new ArrayList<>(confElemList.size());
        for(Element elem : confElemList) {
            confList.add(elem.getAttribute("name"));
        }
        return confList;
    }

    public static Map<String, List<Integer>> parseIvyConfVersions(List<String> ivyConfs) {
        Map<String, List<Integer>> map = new LinkedHashMap<>();
        Pattern pat = Pattern.compile("^(.*?)(-jdk(\\d+))$");
        for(String conf : ivyConfs) {
            Matcher m = pat.matcher(conf);
            if (m.matches()) {
                String confPart = m.group(1);
                Integer version = Integer.parseInt(m.group(3));
                List<Integer> versions = map.get(confPart);
                if (versions == null) {
                    versions = new ArrayList<>();
                    versions.add(version);
                    map.put(conf, versions);
                } else {
                    if (!versions.contains(version)) {
                        versions.add(version);
                    }
                }
            } else {
                List<Integer> versions = map.get(conf);
                if (versions == null) {
                    versions = new ArrayList<>();
                    versions.add(0);
                    map.put(conf, versions);
                } else {
                    if (!versions.contains(0)) {
                        versions.add(0, 0);
                    }
                }
            }
        }
        for(Map.Entry<String, List<Integer>> entry : map.entrySet()) {
            entry.getValue().sort(null);
        }
        return map;
    }

    public static Collection<String> getVersionedIvyConfs(Collection<String> inConfs, Map<String, List<Integer>> ivyConfs, int majorVer) throws IOException {
        Set<String> newConfs = new LinkedHashSet<>();
        for(String conf : inConfs) {
            List<Integer> versions = ivyConfs.get(conf);
            if (versions != null) {
                int supportedVer = 0;
                for(int ver : versions) {
                    if (majorVer >= ver) {
                        supportedVer = ver;
                    }
                }
                if (versions.contains(0)) {
                    newConfs.add(conf); // itself
                }
                if (supportedVer > 0) {
                    newConfs.add(conf+"-jdk"+supportedVer); // closest jdk version
                }
            } else {
                newConfs.add(conf); // shouldn't happen, but let it through
            }
        }
        return newConfs;
    }

    public static Collection<String> getVersionedIvyConfs(Collection<String> inConfs, String ivyConfPath, int majorVer) throws IOException {
        return getVersionedIvyConfs(inConfs, parseIvyConfVersions(getIvyConfNames(ivyConfPath)), majorVer);
    }

    private static List<? extends Element> childElementList(Element element, String childElementName) {
        if (element == null) {
            return null;
        }
        List<Element> elements = new LinkedList<>();
        Node node = element.getFirstChild();
        if (node != null) {
            do {
                String nodeName = getNodeNameIgnorePrefix(node);
                if (node.getNodeType() == Node.ELEMENT_NODE && (childElementName == null ||
                        childElementName.equals(nodeName))) {
                    Element childElement = (Element) node;
                    elements.add(childElement);
                }
            } while ((node = node.getNextSibling()) != null);
        }
        return elements;
    }

    private static String getNodeNameIgnorePrefix(Node node){
        if (node == null) {
            return null;
        }
        String nodeName = node.getNodeName();
        if (nodeName.contains(":")) {
            nodeName = nodeName.split(":")[1];
        }
        return nodeName;
    }

    public static void main(String[] args) {
        for(int i = 0; i < args.length; i++) {
            String arg = args[i];
            if ("-majorver".equals(arg)) {
                System.out.println(getJavaVersionMajor());
                return;
            } else if ("-ivyverconf".equals(arg)) {
                try {
                    String confString = args[i+1];
                    String ivyConfFile = args[i+2];
                    Collection<String> newConfs = getVersionedIvyConfs(Arrays.asList(confString.split(",")),
                            ivyConfFile, getJavaVersionMajor());
                    if (newConfs != null && !newConfs.isEmpty()) {
                        System.out.println(String.join(",", newConfs));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return;
            }
        }
    }
}
