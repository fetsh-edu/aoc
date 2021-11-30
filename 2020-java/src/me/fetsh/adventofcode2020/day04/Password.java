package me.fetsh.adventofcode2020.day04;

import me.fetsh.adventofcode2020.utils.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Password {
    static ArrayList<String> validFields = new ArrayList<>(Arrays.asList("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"));
    static ArrayList<String> requiredFields = validFields.stream().filter(el -> !el.equals("cid")).collect(Collectors.toCollection(ArrayList::new));
    HashMap<String, String> password;

    Password(String... fields) {
        this.password = new HashMap<>(Arrays.stream(fields).map(f -> f.split(":")).filter(a -> validFields.contains(a[0])).collect(Collectors.toMap(data -> data[0], data -> data[1])));
    }

    static Password parse(String pass){
        return new Password(Arrays.stream(pass.split("\\s+")).filter(s -> s.contains(":")).toArray(String[]::new));
    }

    boolean isValidPart1() {
        return password.keySet().containsAll(requiredFields);
    }

    boolean isValidPart2() {
        return isValidPart1() &&
                isValidNumber(password.get("byr"), 1920, 2002) &&
                isValidNumber(password.get("iyr"), 2010, 2020) &&
                isValidNumber(password.get("eyr"), 2020, 2030) &&
                isValidHGT() &&
                password.get("hcl").matches("#([0-9]|[a-f]){6}") &&
                password.get("ecl").matches("(amb|blu|brn|gry|grn|hzl|oth)") &&
                password.get("pid").matches("[0-9]{9}");

    }

    boolean isValidHGT(){
        var matcher = Pattern.compile("\\A(\\d+)(cm|in)\\z").matcher(password.get("hgt"));
        if (!matcher.matches()) return false;
        return matcher.group(2).equals("cm")
                ? isValidNumber(matcher.group(1), 150, 193)
                : isValidNumber(matcher.group(1), 59, 76);
    }

    boolean isValidNumber(String intString, int min, int max){
        if(!intString.matches("\\d+")) return false;
        int intInt = Integer.parseInt(intString);
        return intInt >= min && intInt <= max;
    }

    public static void main(String[] args) throws IOException {
        var passwords = File.readAllBytes(Password.class.getResource("input.txt").getPath()).split("\n\n");

        System.out.println("valid1: " + Arrays.stream(passwords).map(Password::parse).filter(Password::isValidPart1).count());
        System.out.println("valid2: " + Arrays.stream(passwords).map(Password::parse).filter(Password::isValidPart2).count());
    }
}
