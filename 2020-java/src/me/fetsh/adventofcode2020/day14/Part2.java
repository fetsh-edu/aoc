package me.fetsh.adventofcode2020.day14;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;

public class Part2 {
    public static void main(String[] args) throws FileNotFoundException {
        var file = Part1.class.getResource("input.txt").getFile();

        String mask = "";
        HashMap<Long, Long> memory = new HashMap<>();
        Scanner sc = new Scanner(new FileInputStream(file));
        while (sc.hasNextLine()) {
            var line = sc.nextLine();
            if (line.startsWith("mask")) {
                mask = line.substring(7);
            } else {
                var splitLine = line.split("\\D+");
                var address = Long.parseLong(splitLine[1]) | Long.parseLong(mask.replaceAll("X", "0"), 2);
                var value = Long.parseLong(splitLine[2]);
                var addresses = new ArrayList<Long>();
                getAddresses(addresses, mask, address);
                for (long a: addresses) {
                    memory.put(a, value);
                }
            }
        }
        sc.close();
        System.out.println(memory.values().stream().mapToLong(s -> s).sum());
    }

    private static void getAddresses(ArrayList<Long> addresses, String mask, long address) {
        if (!mask.contains("X")) {
            addresses.add(address);
        } else {
            var ofXIndex = mask.indexOf("X");
            var addressString = String.format("%1$" + 36 + "s", Long.toBinaryString(address)).replace(' ', '0');
            var newMask = mask.replaceFirst("X", "Y");
            getAddresses(addresses, newMask, Long.parseLong(addressString.substring(0, ofXIndex) + "1" + addressString.substring(ofXIndex + 1), 2));
            getAddresses(addresses, newMask, Long.parseLong(addressString.substring(0, ofXIndex) + "0" + addressString.substring(ofXIndex + 1), 2));
        }
    }
}
