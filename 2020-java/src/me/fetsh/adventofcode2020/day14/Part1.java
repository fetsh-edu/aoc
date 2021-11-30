package me.fetsh.adventofcode2020.day14;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Scanner;

public class Part1 {
    public static void main(String[] args) throws FileNotFoundException {
        var file = Part1.class.getResource("input.txt").getFile();

        HashMap<Integer, Long> memory = new HashMap<>();
        String mask = "";

        Scanner sc = new Scanner(new FileInputStream(file));
        while (sc.hasNextLine()) {
            var line = sc.nextLine();
            if (line.startsWith("mask")) {
                mask = line.substring(7);
            } else {
                var address = Integer.parseInt(line.split("\\D+")[1]);
                var value = Long.parseLong(line.split("\\D+")[2]);
                memory.put(address, applyMaskToValue(mask, value));
            }
        }
        sc.close();
        System.out.println(memory.values().stream().mapToLong(s -> s ).sum());
    }

    public static long applyMaskToValue(String mask, long value) {
        return (value
                & Long.parseLong(mask.replaceAll("X", "1"),2))
                    | Long.parseLong(mask.replaceAll("X", "0"), 2);
    }

}
