import sys
import io
from pda import PDA

if __name__ == "__main__":
    input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    lines = input_stream.readlines()
    pda = PDA.parse(lines)
    grammar = pda.to_cfg()
    sys.stdout.buffer.write((str(grammar) + "\n").encode('utf-8'))
