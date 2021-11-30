package me.fetsh.adventofcode2020.day19;

import io.vavr.collection.List;
import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.HashMap;
import java.util.regex.Pattern;

public class MonsterMessages {
    public static void main(String[] args) throws IOException {
        var input = File.readAllBytes(MonsterMessages.class.getResource("input.txt").getFile());
        var rules = new HashMap<Integer, String>();
        List.of(input.split("\n\n")[0].split("\n")).forEach(s -> rules.put(Integer.parseInt(s.split(": ")[0]),  s.split(": ")[1]));
        var messages = List.of(input.split("\n\n")[1].split("\n"));

        System.out.println(part1(rules, messages));

        rules.put(8, "42 | 42 8");
        rules.put(11, "42 31 | 42 11 31");
        System.out.println(part1(rules, messages));

    }

    static int part1(HashMap<Integer, String> rules, List<String> messages){
        Pattern rule0 = Pattern.compile(compile(rules, 0, 0));
        return messages.filter(m -> rule0.matcher(m).matches()).size();
    }

    static String compile(HashMap<Integer, String> input, int index, int limit){
        if (limit > 20) return "";
        var rule = input.get(index);
        if (rule.startsWith("\"")) return rule.substring(1,2);
        return List.of(rule.split("\\s+\\|\\s+"))
                .map(r -> List.of(r.split(" ")).map(s -> "(".concat( compile(input, Integer.parseInt(s), limit + 1) ).concat(")")).mkString())
                .mkString("(", "|", ")" );
    }
}
