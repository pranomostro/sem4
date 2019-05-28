import java.util.Scanner;
import java.util.TreeSet;
import java.util.Set;

public class Test {

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		Set<Character> alphabet = new TreeSet<>();
    	String chars = sc.nextLine();
	for (char c : chars.toCharArray()) {
		if (c != ';') alphabet.add(c);
	}

        Regex r = Regex.parse(sc.nextLine());
        System.out.println(r.toEpsilonNFA(alphabet));
        
		sc.close();
	}

}
