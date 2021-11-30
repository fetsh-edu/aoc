package me.fetsh.adventofcode2020.day01;

import me.fetsh.adventofcode2020.utils.File;

import java.io.IOException;
import java.util.Arrays;

public class Main {
    public static void main(String[] args) throws IOException {
        var expensesFileContent = File.readAllBytes(Main.class.getResource("input.txt").getPath());
        var expenses = Arrays.stream(expensesFileContent.split("\n")).mapToInt(Integer::parseInt).toArray();
        int result = 0;
        for (int i = 0; i < expenses.length - 2; i++) {
            for (int j = i + 1; j < expenses.length - 1; j++) {
                for (int k = j + 1; k < expenses.length; k++) {
                    if (expenses[i] + expenses[j] + expenses[k] == 2020){
                        result = expenses[i] * expenses[j] * expenses[k];
                        break;
                    }
                }
            }
        }
        System.out.println(result);
    }
}
