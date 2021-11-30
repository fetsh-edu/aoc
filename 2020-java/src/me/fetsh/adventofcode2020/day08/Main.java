package me.fetsh.adventofcode2020.day08;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.IntStream;

record Instruction(String instruction){
    private static final Map<String, BiFunction<Integer, Integer, Integer>> accFunction = new HashMap<>();
    private static final Map<String, BiFunction<Integer, Integer, Integer>> indFunction = new HashMap<>();
    static {
        accFunction.put("nop", (a, b) -> a);
        accFunction.put("jmp", (a, b) -> a);
        accFunction.put("acc", Integer::sum);
        indFunction.put("nop", (a, b) -> a + 1);
        indFunction.put("jmp", Integer::sum);
        indFunction.put("acc", (a, b) -> a + 1);
    }
    int acc(int init) { return accFunction.get(cmd()).apply(init, value()); }
    int index(int init) { return indFunction.get(cmd()).apply(init, value()); }
    int value() { return Integer.parseInt(instruction.substring(4)); }
    String cmd() { return instruction.substring(0,3); }

    Instruction flip(Boolean _flip){
        if (!_flip) return this;
        if (instruction.startsWith("acc")) return this;
        return instruction.startsWith("jmp")
                ? new Instruction(instruction.replace("jmp", "nop"))
                : new Instruction(instruction.replace("nop", "jmp"));
    }
}

public class Main {
    public static void main(String[] args) throws IOException {
        var instructionLines = File.readAllBytes(Main.class.getResource("input.txt").getPath()).split("\n");

        // Part 1
        calcAccBeforeRepeating(instructionLines, new HashSet<>(), 0, 0)
                .ifPresentOrElse(System.out::println, () -> System.out.println("Nope"));

        // Part 2
        IntStream.range(0, instructionLines.length)
                .filter(i -> !instructionLines[i].startsWith("acc"))
                .mapToObj(i -> calcSuccessfulAcc(instructionLines, new HashSet<>(), 0, i,0))
                .filter(OptionalInt::isPresent)
                .map(OptionalInt::getAsInt)
                .findFirst()
                .ifPresentOrElse(System.out::println, () -> System.out.println("Nope"));
    }

    static OptionalInt calcAccBeforeRepeating(String[] instructionLines, Set<Integer> visitedIndices, Integer index, Integer acc){
        if (index >= instructionLines.length) return OptionalInt.empty();
        if (!visitedIndices.add(index)) return OptionalInt.of(acc);
        var instruction = new Instruction(instructionLines[index]);
        return calcAccBeforeRepeating(instructionLines, visitedIndices, instruction.index(index), instruction.acc(acc));
    }

    static OptionalInt calcSuccessfulAcc(String[] instructionLines, Set<Integer> visitedIndices, Integer index, Integer indToFix, Integer acc){
        if (index == instructionLines.length) return OptionalInt.of(acc);
        if (index > instructionLines.length) return OptionalInt.empty();
        if (!visitedIndices.add(index)) return OptionalInt.empty();
        var instruction = new Instruction(instructionLines[index]).flip(index.equals(indToFix));
        return calcSuccessfulAcc(instructionLines, visitedIndices, instruction.index(index), indToFix, instruction.acc(acc));
    }
}