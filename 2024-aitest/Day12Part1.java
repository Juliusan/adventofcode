import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Būčiau daręs Erlangu, bet Gemini nemoka :-( Prireikė 11 bandymų. Po pirmojo
 *  bandymo nustebino, nes du pavyzdžius gerai pasiėmė. Tačiau parašytas kodas
 *  grąžindavo nulius. Liepiau pasitaisyti, tai vis pasitaisydavo į blogus
 *  variantus. Galiausiai liepiau prisidėti trečią pavyzdį, kurį nuskaitė kiek
 *  netiksliai - pataisiau ranka. Ir tada po dar kelių variantų išpjovė tris
 *  teisingus atsakymus. Tada paprašiau nuskaityti realią užduotį iš failo,
 *  jis vietoj to parašė tą darantį kodą, kuris iš karto davė gerą atsakymą.
 */

public class Day12Part1 {

    private static final int[][] DIRECTIONS = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    public static int calculateTotalFencePrice(char[][] garden) {
        int rows = garden.length;
        int cols = garden[0].length;
        Map<Character, List<Integer>> regionPrices = new HashMap<>();

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                char plantType = garden[i][j];
                if (garden[i][j] != '\0') {
                    Set<String> visited = new HashSet<>();
                    int[] perimeter = {0};
                    dfs(garden, i, j, plantType, visited, perimeter);
                    List<Integer> prices = regionPrices.getOrDefault(plantType, new ArrayList<>());
                    prices.add(visited.size() * perimeter[0]);
                    regionPrices.put(plantType, prices);
                }
            }
        }

        return regionPrices.values().stream()
                .flatMapToInt(list -> list.stream().mapToInt(Integer::intValue))
                .sum();
    }


    private static void dfs(char[][] garden, int row, int col, char plantType, Set<String> visited, int[] perimeter) {
        if (row < 0 || row >= garden.length || col < 0 || col >= garden[0].length ||
                garden[row][col] != plantType || visited.contains(row + "," + col)) {
            return;
        }

        visited.add(row + "," + col);  //No change here

        for (int[] dir : DIRECTIONS) {
            int newRow = row + dir[0];
            int newCol = col + dir[1];

             // Mark the current cell visited *BEFORE* the recursive calls
             if(!visited.contains(newRow+","+newCol)) {
                if (newRow < 0 || newRow >= garden.length || newCol < 0 || newCol >= garden[0].length ||
                        garden[newRow][newCol] != plantType) {
                     perimeter[0]++;
                 }
             }


            dfs(garden, newRow, newCol, plantType, visited, perimeter);
           //Move this here to avoid counting fences before visiting neighbors
            garden[row][col] = '\0';
        }
    }

    public static void main(String[] args)  throws IOException {
        char[][] garden1 = {
                {'A', 'A', 'A', 'A'},
                {'B', 'B', 'C', 'D'},
                {'B', 'B', 'C', 'C'},
                {'E', 'E', 'E', 'C'}
        };
        System.out.println("Total price for garden1: " + calculateTotalFencePrice(garden1)); // Expected: 140

        char[][] garden2 = {
                {'O', 'O', 'O', 'O', 'O'},
                {'O', 'X', 'O', 'X', 'O'},
                {'O', 'O', 'O', 'O', 'O'},
                {'O', 'X', 'O', 'X', 'O'},
                {'O', 'O', 'O', 'O', 'O'}
        };
        System.out.println("Total price for garden2: " + calculateTotalFencePrice(garden2)); // Expected: 772


        char[][] garden3 = {
            {'R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'F', 'F'},
            {'R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'C', 'F'},
            {'V', 'V', 'R', 'R', 'R', 'C', 'C', 'F', 'F', 'F'},
            {'V', 'V', 'R', 'C', 'C', 'C', 'J', 'F', 'F', 'F'},
            {'V', 'V', 'V', 'V', 'C', 'J', 'J', 'C', 'F', 'E'},
            {'V', 'V', 'I', 'V', 'C', 'C', 'J', 'J', 'E', 'E'},
            {'V', 'V', 'I', 'I', 'I', 'C', 'J', 'J', 'E', 'E'},
            {'M', 'I', 'I', 'I', 'I', 'I', 'J', 'J', 'E', 'E'},
            {'M', 'I', 'I', 'I', 'S', 'I', 'J', 'E', 'E', 'E'},
            {'M', 'M', 'M', 'I', 'S', 'S', 'J', 'E', 'E', 'E'}
        };
        System.out.println("Total price for garden3: " + calculateTotalFencePrice(garden3)); // Expected: 1930

        List<String> lines = Files.readAllLines(Paths.get("day12.txt"));
        char[][] garden4 = new char[lines.size()][lines.get(0).length()];
        for (int i = 0; i < lines.size(); i++) {
            for (int j = 0; j < lines.get(i).length(); j++) {
                garden4[i][j] = lines.get(i).charAt(j);
            }
        }

        System.out.println("Total price for garden4 (from file): " + calculateTotalFencePrice(garden4));

    }
}