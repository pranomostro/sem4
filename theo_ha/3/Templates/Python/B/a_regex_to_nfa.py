from regex import Regex_Parser
import sys
import io

# Main function -- can be ignored
if __name__ == "__main__":
    input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    lines = input_stream.readlines()
    if len(lines) < 2:
        print("Invalid input: expecting more lines!")
        sys.exit(1)
    alphabet = lines[0].strip().split(";")
    rp = Regex_Parser(lines[1])
    r = rp.parse()
    print(r.to_epsilon_nfa(alphabet))
