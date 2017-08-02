package org.ofbiz.base.util;

import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * SCIPIO: Randomization utilities.
 */
public class UtilRandom {

    /**
     * Method should generate random number that represents a time between two
     * dates.
     * 
     * @return
     */
    private static long getRandomTimeBetweenTwoDates(Timestamp beginDate, Map<String, Object> context) {
        long beginTime, endTime;
        Calendar cal = Calendar.getInstance();

        if (context.get("maxDate") != null) {
            endTime = ((Timestamp) context.get("maxDate")).getTime();
        } else {
            endTime = cal.getTimeInMillis();
        }

        if (beginDate != null) {
            beginTime = beginDate.getTime();
        } else if (context.get("minDate") != null) {
            beginTime = ((Timestamp) context.get("minDate")).getTime();
        } else {
            cal.add(Calendar.DATE, -180);
            beginTime = cal.getTimeInMillis();
        }
        long diff = endTime - beginTime + 1;
        beginTime = beginTime + (long) (Math.random() * diff);
        return beginTime;
    }

    public static String generateRandomDate(Map<String, Object> context) {
        return generateRandomDate(null, context);
    }

    public static String generateRandomDate(Date beginDate, Map<String, Object> context) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date randomDate;
        if (UtilValidate.isNotEmpty(beginDate)) {
            randomDate = new Date(getRandomTimeBetweenTwoDates(new Timestamp(beginDate.getTime()), context));
        } else {
            randomDate = new Date(getRandomTimeBetweenTwoDates(null, context));
        }
        return dateFormat.format(randomDate);
    }

    public static Timestamp generateRandomTimestamp(Map<String, Object> context) {
        return generateRandomTimestamp(null, context);
    }

    public static Timestamp generateRandomTimestamp(Date beginDate, Map<String, Object> context) {
        Timestamp randomDate;
        if (UtilValidate.isNotEmpty(beginDate)) {
            randomDate = new Timestamp(getRandomTimeBetweenTwoDates(new Timestamp(beginDate.getTime()), context));
        } else {
            randomDate = new Timestamp(getRandomTimeBetweenTwoDates(null, context));
        }
        return randomDate;
    }
    
    static String generateRandomPassword(int len){
        System.out.println("Your Password ");
        String charsCaps="ABCDEFGHIJKLMNOPQRSTUVWXYZ"; 
        String Chars="abcdefghijklmnopqrstuvwxyz";
        String nums="0123456789";
        String symbols="!@#$%^&*()_+-=.,/';:?><~*/-+";
        String passSymbols=charsCaps + Chars + nums +symbols;
        Random rnd=new Random();
        char[] password=new char[len];

        for(int i=0; i<len;i++){
            password[i]=passSymbols.charAt(rnd.nextInt(passSymbols.length()));
        }
      return password.toString();

    }

    public static int random(List myList) {
        int size = myList.size();
        int index = new Random().nextInt(size);
        return index;
    }

    public static boolean getRandomBoolean() {
        return Math.random() < 0.5;
    }

    public static int getRandomEvenInt(int min, int max) {
        int x = getRandomInt(min, max);
        while (x % 2 != 0) {
            x = getRandomInt(min, max);
        }
        return x;
    }

    public static int getRandomInt(int min, int max) {
        Random rand = new Random();
        int x = rand.nextInt(max - min + 1) + min;
        x = rand.nextInt(max - min + 1) + min;
        return x;
    }

    public static String generateRandom() {        
        return new BigInteger(130, new SecureRandom()).toString(32);
    }

}
