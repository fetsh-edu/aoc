package me.fetsh.adventofcode2020.day18;

import io.vavr.collection.List;
import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.HashMap;
import java.util.function.ToLongBiFunction;
import java.util.regex.Pattern;

public class OperationOrder {
    public static void main(String[] args) throws IOException {
        var input = File.readAllBytes(OperationOrder.class.getResource("input.txt").getPath());

        Long part1 = List.of(input.split("\n"))
                .map(OperationOrder::compute).foldLeft(0L, Long::sum);

        Long part2 = List.of(input.split("\n"))
                .map(OperationOrder::compute2).foldLeft(0L, Long::sum);

        System.out.println(part1);
        System.out.println(part2);

    }


    static HashMap<String, ToLongBiFunction<Long, Long>> methods = new HashMap<>();
    static {
        methods.put("+", Long::sum);
        methods.put("*", (a, b) -> a * b);
    }

    static Pattern parens = Pattern.compile("\\(([^()]+)\\)");
    static Pattern simpleComp  = Pattern.compile("(\\d+) ([*+]) (\\d+)");
    public static long compute(String string) {
        var m = parens.matcher(string);
        var m2 = simpleComp.matcher(string);
        if(m.find()){
            return  compute(m.replaceFirst(Long.toString(compute(m.group(1)))));
        } else if (m2.find()){
            return compute(m2.replaceFirst(Long.toString(methods.get(m2.group(2)).applyAsLong(Long.parseLong(m2.group(1)), Long.parseLong(m2.group(3))))));
        } else {
            return Long.parseLong(string);
        }
    }

    static Pattern sum  = Pattern.compile("(\\d+) ([+]) (\\d+)");
    static Pattern multiply  = Pattern.compile("(\\d+) ([*]) (\\d+)");
    public static long compute2(String string) {
        System.out.println(string);
        var m1 = parens.matcher(string);
        var m2 = sum.matcher(string);
        var m3 = multiply.matcher(string);
        if(m1.find()){
            return  compute2(m1.replaceFirst(Long.toString(compute2(m1.group(1)))));
        } else if (m2.find()){
            return compute2(m2.replaceFirst(Long.toString(methods.get("+").applyAsLong(Long.parseLong(m2.group(1)), Long.parseLong(m2.group(3))))));
        } else if (m3.find()) {
            return compute2(m3.replaceFirst(Long.toString(methods.get("*").applyAsLong(Long.parseLong(m3.group(1)), Long.parseLong(m3.group(3))))));
        } else {
            return Long.parseLong(string);
        }
    }
}
